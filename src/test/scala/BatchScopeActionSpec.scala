package org.positronicnet.orm.batchtest

import org.positronicnet.db._
import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.notifications.Actions._
import org.positronicnet.content.{ContentProviderQuery, 
                                  PositronicContentResolver}

import org.positronicnet.test._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.xtremelabs.robolectric.Robolectric

import org.positronicnet.orm.variantTest._ // reuse variant test fixtures...

// Also import test mapping of 'CallLog'
import android.provider.CallLog
import org.positronicnet.test.providerspec.Calls
import org.positronicnet.orm.test.{CallLogEntry, CallLogEntries}

import android.net.Uri
import android.content.ContentValues
import scala.collection.mutable.{HashMap, ArrayBuffer}

// Set up a mock version of the ContentProviderOperation machinery
// which lets us get at the innards (and do operations on things 
// under our control).  Which makes this one of the rare cases where
// it isn't total nonsense to test the mocks (because what's really
// under test is the ScopeBatchTranslation mixin).

class MockBatchScopeAction
  extends ScopeBatchTranslation
{
  class MockOperation( val opType: String, val uri: Uri ) {

    var selection: String = null
    var selectionArgs: Array[String] = null

    val values = new ContentValues
    val backrefs = new HashMap[ String, Int ]
  }

  type Operation = MockOperation

  val ops = new ArrayBuffer[ MockOperation ]

  def addOperation( op: Operation ) = { ops += ( op ) }
  def numPriorOps = ops.size

  def newInsert( uri: Uri ) = new MockOperation( "insert", uri )
  def newUpdate( uri: Uri ) = new MockOperation( "update", uri )
  def newDelete( uri: Uri ) = new MockOperation( "delete", uri )

  def addValues( op: Operation, cv: ContentValues ): Unit =
    op.values.putAll( cv )

  def addValueBackReference( op: Operation, key: String, backref: Int ) =
    op.backrefs( key ) = backref

  def addSelection( op: Operation, sel: String, selArgs: Array[String] ): Unit =
    if (op.selection != null)
      throw new RuntimeException( "two selections for op" )
    else {
      op.selection = sel
      op.selectionArgs = selArgs
    }

  // Need this to keep ops on DB-backed records from blowing up...

  override def mgrContentUri( mgr: BaseRecordManager[_] ) =
    mgr.baseQuery match {
      case crQuery: ContentProviderQuery[_] => super.mgrContentUri (mgr )
      case _ => Uri.parse( "http://android.com" )
    }

}

class BatchScopeActionSpec
  extends Spec 
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
  with SerializationTestHelpers
{
  override def beforeEach = {
    PositronicContentResolver.openInContext( Robolectric.application )
    PeopleDb.openInContext( Robolectric.application )
    PeopleDb.setupFixtures
  }

  override def afterEach = {
    PeopleDb.close
    PositronicContentResolver.close
  }

  describe ("batch of insertions") {

    lazy val insertBatch = {

      val insertBatch = new MockBatchScopeAction
      val school = new School( "Fish School" )
      val school2 = new School( "School of Rock" )

      insertBatch.add( Save( school ))
      insertBatch.add( Save( Teacher( "Charlie Tuna", 42, school.id )))
      insertBatch.add( Save( Student( "Nemo Clownfish", 2012, school.id )))
      insertBatch.add( Save( school2 ))
      insertBatch.add( Save( Teacher( "Jack Black", 21, school2.id )))

      insertBatch
    }

    it ("should have the right number and type of ops") {
      insertBatch.ops should have size (5)
      insertBatch.ops.map{ _.opType }.toSeq should equal (
        Seq("insert", "insert", "insert", "insert", "insert"))
    }

    it ("should save simple content values") {
      insertBatch.ops(0).values.getAsString("name") should be ("Fish School")
      insertBatch.ops(1).values.getAsString("name") should be ("Charlie Tuna")
      insertBatch.ops(2).values.getAsString("name") should be ("Nemo Clownfish")

      insertBatch.ops(1).values.getAsInteger("rating")     should be (42)
      insertBatch.ops(2).values.getAsInteger("class_year") should be (2012)
    }

    it ("should save variant discriminators") {
      insertBatch.ops(1).values.getAsString("person_type") should be ("teacher")
      insertBatch.ops(2).values.getAsString("person_type") should be ("student")
    }

    it ("should back-patch IDs") {
      insertBatch.ops(1).backrefs("school_id") should be (0)
      insertBatch.ops(2).backrefs("school_id") should be (0)
      insertBatch.ops(4).backrefs("school_id") should be (3)
    }
  }

  describe ("batch updates and deletes") {

    def studentByName( name: String ) =
      (People.students.whereEq( "name" -> name ).fetchOnThisThread)(0)

    lazy val lucy  = studentByName( "Lucy van Pelt" )
    lazy val linus = studentByName( "Linus van Pelt" )
    lazy val sally = studentByName( "Sally Brown" )

    lazy val updateBatch = {
      val updateBatch = new MockBatchScopeAction
      updateBatch.add( Save( lucy.copy( classYear = 2015 )))
      updateBatch.add( Save( linus.copy( classYear = 2017 )))
      updateBatch.add( Delete( sally ))
      updateBatch
    }

    it ("should have the right number and type of ops") {
      updateBatch.ops should have size (3)
      updateBatch.ops.map{ _.opType }.toSeq should equal (
        Seq("update", "update", "delete"))
    }

    it ("should save content values and ids for updates") {

      val lucyUpdateVals  = updateBatch.ops(0).values
      val linusUpdateVals = updateBatch.ops(1).values

      lucyUpdateVals.getAsInteger( "class_year" )  should equal (2015)
      linusUpdateVals.getAsInteger( "class_year" ) should equal (2017)

      lucyUpdateVals.getAsLong( "school_id" )  should equal (lucy.schoolId.id)
      linusUpdateVals.getAsLong( "school_id" ) should equal (linus.schoolId.id)
    }

    it ("should add correct selectors") {
      updateBatch.ops.map{ _.selection }.toSeq should equal (
        Seq( "_id=?", "_id=?", "_id=?" ))
      updateBatch.ops(0).selectionArgs should equal(Array(lucy.id.id.toString))
      updateBatch.ops(1).selectionArgs should equal(Array(linus.id.id.toString))
      updateBatch.ops(2).selectionArgs should equal(Array(sally.id.id.toString))
    }
  }

  describe ("URL mappings to content providers") {
    it ("should have correct value") {
      val batch = new MockBatchScopeAction
      val entry = new CallLogEntry( 2, "foo", "bar", 7 ) // nonsense; irrelevant
      batch.add (Save (entry))
      batch.ops(0).uri should equal (Calls.CONTENT_URI)
    }
  }
}

