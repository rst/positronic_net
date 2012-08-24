package org.positronicnet.orm.test

import org.positronicnet.db._
import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.notifications.Actions._

import org.positronicnet.test._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class SingleThreadOrmSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
  with SerializationTestHelpers
{
  describe( "Single-thread ORM queries" ){

    def assertIsUndoneItems( items: Seq[TodoItem] ) = {
      items should have size (2)
      assert( haveItem( "wash dog" , false, items ))
      assert( haveItem( "feed dog" , false, items ))
    }

    it ("should find all the records") {
      val results = TodoItems.fetchOnThisThread
      results should have size (3)

      assert( haveItem( "wash dog" , false, results ))
      assert( haveItem( "feed dog" , false, results ))
      assert( haveItem( "walk dog" , true,  results ))
    }
    it ("should retrieve only matching records with conds"){
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread
      assertIsUndoneItems( undoneItems )
    }
    it ("should also handle conds using java field names"){
      val undoneItems = TodoItems.whereEq( "isDone" -> false ).fetchOnThisThread
      assertIsUndoneItems( undoneItems )
    }
    it ("should support full-sql matching syntax"){
      val undoneItems = TodoItems.where( "is_done=?", false ).fetchOnThisThread
      assertIsUndoneItems( undoneItems )
    }
  }

  describe( "Record ID equality" ) {
    it ("IDs of found records should be distinct") {
      val items = TodoItems.fetchOnThisThread
      items(0).id should not equal (items(1).id)
    }
    it ("IDs of found records should not be null") {
      val items = TodoItems.fetchOnThisThread
      items(0).id should not equal (TodoItems.nullId)
    }
    it ("multiple null IDs should compare equal") {
      TodoItems.nullId should equal (TodoItems.nullId)
    }
  }

  describe( "Single-thread ORM find" ) {
    it ("should find the right records") {
      for (item <- TodoItems.fetchOnThisThread) {
        TodoItems.findOnThisThread( item.id ) should equal (item)
        TodoItems.onThisThread( Find( item.id ){ _ should equal (item)} )
      }
    }
  }

  describe( "Single-thread ORM count") {
    it ("should count all records with no conds") {
      TodoItems.count.fetchOnThisThread should equal (3)
    }
    it ("should count the right records with conds") {
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> true  ).count.fetchOnThisThread should equal (1)
    }
  }

  describe( "Single-thread ORM delete via scope" ) {
    it ("should eliminate selected records") {
      TodoItems.whereEq( "is_done" -> true ).onThisThread( DeleteAll )
      TodoItems.count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM delete via record selection" ) {
    it ("should kill the selected record") {
      val doneItems = TodoItems.whereEq( "is_done" -> true ).fetchOnThisThread
      doneItems should have size (1)

      doneItems.foreach{ it => TodoItems.onThisThread( Delete( it )) }

      TodoItems.count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM update via scope" ) {
    it ("should change counts") {
      TodoItems.whereEq( "description" -> "feed dog" ).onThisThread( UpdateAll("is_done" -> true))
      TodoItems.whereEq( "is_done" -> true ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM update via record selection" ) {

    def doUpdate = {
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread
      undoneItems should have size (2)

      val doneItem = undoneItems( 0 ).isDone( true )
      TodoItems.onThisThread( Save( doneItem ))
                      
      doneItem
    }

    it ("should change counts") {
      doUpdate
      TodoItems.count.fetchOnThisThread should equal (3)
      TodoItems.whereEq( "is_done" -> true  ).count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (1)
    }
      
    it ("should alter the record") {
      val changedItem = doUpdate
      val doneItems = TodoItems.fetchOnThisThread
      assert( haveItem( changedItem.description, true, doneItems ))
    }
  }

  describe( "Single-thread ORM insert" ) {

    def doInsert = TodoItems.onThisThread( Save( TodoItem("train dog", false )))
    
    it ("should alter counts") {
      doInsert
      TodoItems.count.fetchOnThisThread should equal (4)
    }

    it ("should make the record show up on queries") {
      doInsert
      val items = TodoItems.fetchOnThisThread
      assert( haveItem( "train dog", false, items ))
      items.find{ _.description == "train dog" }.map{ 
        _.id should not equal( -1 )
      }
    }

    describe ("IDs on insert") {

      def testSetup = {
        val record = TodoItem( "train dog", false )
        TodoItems.onThisThread( Save( record ))
        record
      }

      it ("should record saved ID") {
        val record = testSetup
        assert (record.id.id < 0)
        assert (record.id.savedId > 0)
      }

      it ("should be able to re-find") {
        val record = testSetup
        val newRec = record.id.fetchOnThisThread

        newRec.description should be ("train dog")
        newRec.isDone should be (false)

        // Note that the original record still has its notionally-immutable
        // unsaved ID, with its exposed long 'id' variable, so it shouldn't
        // compare equal to a query result.  Then again, contents may "shift
        // in transit" for a variety of reasons --- canonicalization of data
        // rows by complex content providers, etc. ...

        newRec.id should not equal (record.id)
        newRec should not equal (record)
      }
      
      it ("should preserve equality of IDs") {
        val record = testSetup
        record.id should equal (new RecordId(TodoItems, record.id.id))
      }

      // ... and, the whole *point* of this rigamarole...

      it ("should enable saves of structures of related records") {

        val newList = TodoList( "fish list" )
        val newItem1 = TodoItem( "feed fish", true, newList.id )
        val newItem2 = newList.items.create.description_:=( "clean bowl" )

        TodoLists.onThisThread( Save (newList)  )
        TodoItems.onThisThread( Save (newItem1) )
        TodoItems.onThisThread( Save (newItem2) )
        
        val savedList = 
          (TodoLists.whereEq("name"->"fish list").fetchOnThisThread)(0)
        val savedItems = savedList.items.order("description").fetchOnThisThread

        savedItems.map(it=>(it.description, it.isDone)).toList should equal (
          List(("clean bowl",false),("feed fish", true)))
      }

    }

  }

  describe( "ORM .order(...) subscopes" ) {
    it ("should give the right answer") {

      val ascItems = TodoItems.order( "description asc" ).fetchOnThisThread
      ascItems.map{ _.description }.toList should equal (
        List("feed dog","walk dog","wash dog"))

      val descItems = TodoItems.order( "description desc" ).fetchOnThisThread
      descItems.map{ _.description }.toList should equal (
        List("wash dog","walk dog","feed dog"))
    }
    it ("should propagate updates up") {
      val orderedItems = TodoItems.order( "description asc" )
      var monitoredCount: Long = -457
      orderedItems ! AddWatcher(this){ items => { monitoredCount = items.size }}
      TodoItems.onThisThread( Save( TodoItem( "crate dog" )))
      monitoredCount should equal (4)
      orderedItems ! StopWatcher(this)
    }
    it ("should propagate updates down") {
      val orderedItems = TodoItems.order( "description asc" )
      var monitoredCount: Long = -457
      TodoItems ! AddWatcher(this){ items => { monitoredCount = items.size }}
      TodoItems.onThisThread( Save( TodoItem( "crate dog" )))
      monitoredCount should equal (4)
      TodoItems ! StopWatcher( this )
    }
  }

  describe( "ORM .limit(...) subscopes" ) {
    it ("should give the right answer") {
      val ascScope = TodoItems.order( "description asc" ).limit(2)
      val ascItems = ascScope.fetchOnThisThread
      ascItems.map{ _.description }.toList should equal (
        List("feed dog","walk dog"))
    }
  }

  describe( "record query support" ) {
    it ("should be set right initially") {
      val qry = TodoItems.recordsQuery( "w%" ){ (s) => 
        TodoItems.where( "description like ?", s ) }
      val records: Seq[ TodoItem ] = qry.fetchOnThisThread
      val descs = records.map{_.description}.toList.sorted
      descs should equal( List( "walk dog", "wash dog" ))
    }
    it ("should be reset on requery") {
      val qry = TodoItems.recordsQuery( "w%" ){ (s) => 
        TodoItems.where( "description like ?", s ) }
      var records = qry.fetchOnThisThread
      qry.watch( this ){ records = _ }
      qry.onThisThread( Requery( "% dog" ))
      val descs = records.map{_.description}.toList.sorted
      descs should equal( List( "feed dog", "walk dog", "wash dog" ))
      qry.stopNotifier( this )
    }
  }

  describe( "count query support" ) {
    it ("should be set right initially") {

      val qry = TodoItems.countQuery( "w%" ){ (s) => 
        TodoItems.where( "description like ?", s ) }

      qry.fetchOnThisThread should equal (2)
    }
    it ("should be reset on requery") {

      val qry = TodoItems.countQuery( "w%" ){ (s) => 
        TodoItems.where( "description like ?", s ) }

      qry.onThisThread( Requery( "% dog" ))
      qry.fetchOnThisThread should equal (3)
    }
  }

  describe( "serialization and deserialization of records and IDs" ) {
    it ("should be able to handle IDs") {
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread
      val tuple = (undoneItems(0).id, undoneItems(1).id)
      assertSerializationPreservesEquality( tuple )
    }
    it ("should be able to handle whole records") {
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread
      val tuple = (undoneItems(0), undoneItems(1))
      assertSerializationPreservesEquality( tuple )
    }
    it ("should be able to use a deserialized ID in a find") {
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread
      val roundtripIdObj = serializationRoundTrip( undoneItems(0).id )
      val roundtripId = roundtripIdObj.asInstanceOf[ RecordId[ TodoItem ]]
      val item = roundtripId.fetchOnThisThread
      item should equal (undoneItems(0))
    }
  }

  // Have access to internals, since the test is in a subpackage
  // of 'orm'...

  describe( "internal --- toDbFieldMemoized" ) {
    it ("should map java field names") {
      // check twice to make sure memoization doesn't screw things up.
      TodoItems.toDbFieldName( "isDone" ) should equal ("is_done")
      TodoItems.toDbFieldName( "isDone" ) should equal ("is_done")
    }
    it ("should leave db field names unchanged") {
      // check twice to make sure memoization doesn't screw things up.
      TodoItems.toDbFieldName( "is_done" ) should equal ("is_done")
      TodoItems.toDbFieldName( "is_done" ) should equal ("is_done")
    }
  }
}
