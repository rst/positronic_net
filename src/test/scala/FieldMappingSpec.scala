package org.positronicnet.orm.test

import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.test.{DbTestFixtures,TodoDb}
import org.positronicnet.util.ReflectUtils

// Variant mapping for TodoItems which "wraps" description.
// (A stand-in for more useful types of transformations...)

case class WrappedString( raw: String )

case class TodoItemWithWrapping ( 
    rawDescription: String  = null, 
    isDone: Boolean = false,
    id: RecordId[TodoItemWithWrapping] = TodoItemsWithWrapping.unsavedId
  )
  extends ManagedRecord
{
  def isDone( newVal: Boolean ) = copy( isDone = newVal )

  lazy val description = WrappedString( rawDescription )
  def setDescription( str: WrappedString ) = 
    this.copy( rawDescription = str.raw )
}

object TodoItemsWithWrapping
  extends RecordManager[ TodoItemWithWrapping ]( TodoDb("todo_items") )
{
  mapField( "rawDescription", "description" )
}

// Test of automatic mapping of tables whose columns are named in
// static fields of some class (e.g., from the Android platform content
// providers, like, say... the call log)

import org.positronicnet.content.PositronicContentResolver
import org.positronicnet.test.providerspec.Calls // mock CallLog.Calls

case class CallLogEntry( callType:   Int    = 0, 
                         number:     String = null, 
                         cachedName: String = null, 
                         date:       Long   = 0,
                         id: RecordId[CallLogEntry] = CallLogEntries.unsavedId )
  extends ManagedRecord

object CallLogEntries extends RecordManagerForFields[ CallLogEntry, Calls ] {
  mapField( "callType", Calls.TYPE )    // override to avoid reserved word
}

// Test of automatic mapping of variant fields from messy content
// providers.  

import android.provider.ContactsContract
import android.provider.ContactsContract.CommonDataKinds

case class Contact (
  val id: RecordId[Contact] = Contacts.unsavedId
) extends ManagedRecord

object Contacts
  extends RecordManagerForFields[ Contact, ContactsContract.Contacts ]

abstract class ContactData extends ManagedRecord {
  val contactId: RecordId[ Contact ]
}

abstract class LabeledData extends ContactData {
  val labelType: Int
}

case class Phone (
  val number:    String            = null,
  val labelType: Int               = -1,
  val label:     String            = null,
  val contactId: RecordId[Contact] = Contacts.unsavedId,
  val id:        RecordId[Phone]   = ContactData.phones.unsavedId
) extends LabeledData

case class Email (
  val address:   String            = null,
  val label:     String            = null,
  val labelType: Int               = -1,
  val contactId: RecordId[Contact] = Contacts.unsavedId,
  val id:        RecordId[Email]   = ContactData.emails.unsavedId
) extends LabeledData

case class UnknownData (
  val mimetype:  String                = null,
  val data1:     String                = null,
  val contactId: RecordId[Contact]     = Contacts.unsavedId,
  val id:        RecordId[UnknownData] = ContactData.unknowns.unsavedId
) extends ContactData

object ContactData
  extends VariantRecordManager[ ContactData ](
    PositronicContentResolver( ContactsContract.Data.CONTENT_URI ),
    "mimetype" // documented value of ContactsContract.DataColumns.MIMETYPE
  )
{
  def dataKindMapper[ TRec <: LabeledData : ClassManifest,
                      TKind : ClassManifest ] = 
    new TaggedVariantForFields[ TRec, TKind ](
      ReflectUtils.getStatic[ String, TKind ]("MIMETYPE")
    ) {
      // Need to special-case the "type" field, since that's a reserved
      // word in Scala.  And since we have trouble accessing the value of
      // ContactsContract.Data.TYPE here (due to "protected static" confusion),
      // we just type it in.  (It is documented, and they're hardly likely
      // changing it would break deployed apps, so it's not likely to happen.)
      mapField( "labelType", "data2" ) 
    }

  val phones = dataKindMapper[ Phone, CommonDataKinds.Phone ] 
  val emails = dataKindMapper[ Email, CommonDataKinds.Email ] 

  val unknowns = 
    new CatchAllVariantForFields[ UnknownData, ContactsContract.Data ]
}

// The actual spec

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class FieldMappingSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  describe( "record with \"wrapped\" column" ) {

    def feedDogItem = {

      // Note *lack* of translation in DB conditions...

      val scope = TodoItemsWithWrapping.whereEq( "description" -> "feed dog" )
      val recs = scope.fetchOnThisThread
      recs should have size (1)
      recs(0)
    }

    it( "should fetch correct values" ) {
      feedDogItem.description should equal (WrappedString( "feed dog" ))
    }
    it( "should handle updates" ) {

      val it = feedDogItem
      val upd = feedDogItem.setDescription( WrappedString("feed and water dog"))
      TodoItemsWithWrapping.onThisThread( Save( upd ))

      val itupd = TodoItemsWithWrapping.findOnThisThread( it.id )
      itupd.description should equal (WrappedString("feed and water dog"))
    }
  }

  describe( "field mapping to column names from a static class" ) {

    def findField( javaName: String, colName: String ) =
      CallLogEntries.fieldsSeq.find{ f => 
        f.recordFieldName == javaName && 
        f.dbColumnName    == colName }

    it( "should automatically map matching fields" ) {
      findField( "number",     Calls.NUMBER      ) should not equal (None)
      findField( "cachedName", Calls.CACHED_NAME ) should not equal (None)
      findField( "date",       Calls.DATE        ) should not equal (None)
    }
    it( "should support overrides" ) {
      findField( "callType",   Calls.TYPE        ) should not equal (None)
    }
    it( "should correctly default the URI" ) {

      Calls.CONTENT_URI should not equal (null)

      // Bletch.
      //
      // Hideous way to write this... but gaining access to the underlying
      // state would get us into access to org.positronicnet.content-private
      // fields.  Sigh...

      val baseQueryToString = CallLogEntries.baseQuery.toString 
      baseQueryToString should include (Calls.CONTENT_URI.toString)
    }
    it( "should map the ID field, if present" ) {
      val idField = Contacts.fieldsSeq.find{ f =>
        f.recordFieldName == "id" &&
        f.dbColumnName    == android.provider.BaseColumns._ID
      }
      idField should not equal (None)
    }
  }

  describe( "field mapping to variants with static-field column specs") {
    
    // Some column names are given here as strings because they're
    // defined by the framework in protected classes and inherited, 
    // and scalac has its usual problems with "protected static" 
    // anything.  The values given here are documented, FWIW.  
    //
    // (The code under test gets the values by runtime reflection,
    // which follows Java rules, and isn't subject to the same
    // restrictions.)

    describe ("a recognized variant") {

      def checkField( javaName: String, colName: String ) = {
        colName should not equal (None)
        ContactData.phones.fieldsSeq.find{ f => 
          f.recordFieldName == javaName && 
          f.dbColumnName    == colName } should not equal (None)
      }

      it( "should automatically map matching fields" ) {
        checkField( "number",    CommonDataKinds.Phone.NUMBER )
        checkField( "label",     "data3"                      )
        checkField( "labelType", "data2"                      )
        checkField( "contactId", "contact_id"                 )
      }
      it( "should map the ID field, if present" ) {
        checkField( "id",     android.provider.BaseColumns._ID )
      }
    }

    describe ("catchall") {

      def checkField( javaName: String, colName: String ) = {
        colName should not equal (None)
        ContactData.unknowns.fieldsSeq.find{ f => 
          f.recordFieldName == javaName && 
          f.dbColumnName    == colName } should not equal (None)
      }

      it( "should automatically map matching fields" ) {
        checkField( "mimetype", "mimetype" )
        checkField( "contactId", "contact_id" )
      }
      it( "should map the ID field, if present" ) {
        checkField( "id", android.provider.BaseColumns._ID )
      }
    }
  }
}
