package org.positronicnet.orm.test

import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.test.{DbTestFixtures,TodoDb}
import org.positronicnet.util.ReflectUtils

import com.xtremelabs.robolectric.Robolectric

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

// Fixtures for testing mapping 'how's (ReadOnly, WriteOnce)...

import org.positronicnet.test.{TodoList,TodoLists}

case class TodoItemSafed( 
  description: String  = null, 
  isDone:      Boolean = false,
  todoListId:  RecordId[TodoList] = TodoLists.unsavedId,
  id:          RecordId[TodoItemSafed] = TodoItemsSafed.unsavedId
)
extends ManagedRecord

object TodoItemsSafed extends RecordManager[ TodoItemSafed ]( TodoDb("todo_items") ){

  // For the sake of testing here, we set up mappings to allow
  // todo_list_id to be set on first save, but to silently ignore
  // attempts to move an existing item from one todo list to another...

  mapField( "todoListId", "todo_list_id", MapAs.WriteOnce )

  // Expose 'dataPairs' for testing...

  override def dataPairs( rec: TodoItemSafed ) = super.dataPairs( rec )
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
  val number:      String            = null,
  val labelType:   Int               = -1,
  val label:       String            = null,
  val contactId:   RecordId[Contact] = Contacts.unsavedId,
  val dataVersion: Int               = -1,
  val id:          RecordId[Phone]   = ContactData.phones.unsavedId
) extends LabeledData

case class Email (
  val address:     String            = null,
  val label:       String            = null,
  val labelType:   Int               = -1,
  val contactId:   RecordId[Contact] = Contacts.unsavedId,
  val dataVersion: Int               = -1,
  val id:          RecordId[Email]   = ContactData.emails.unsavedId
) extends LabeledData

case class UnknownData (
  val mimetype:    String                = null,
  val data1:       String                = null,
  val contactId:   RecordId[Contact]     = Contacts.unsavedId,
  val id:          RecordId[UnknownData] = ContactData.unknowns.unsavedId
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
      ReflectUtils.getStatic[ String, TKind ]("CONTENT_ITEM_TYPE")
    ) {
      // Need to special-case the "type" field, since that's a reserved
      // word in Scala.  And since we have trouble accessing the value of
      // ContactsContract.Data.TYPE here (due to "protected static" confusion),
      // we just type it in.  (It is documented, and they're hardly likely
      // changing it would break deployed apps, so it's not likely to happen.)
      mapField( "labelType", "data2" ) 

      // "data version" is read-only...
      mapField( "dataVersion",
                ReflectUtils.getStatic[ String, ContactsContract.Data ]("DATA_VERSION"),
                MapAs.ReadOnly )
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
  with BeforeAndAfterEach
  with ShouldMatchers
  with DbTestFixtures
{
  override def beforeEach = {
    TodoDb.openInContext( Robolectric.application )
    TodoDb.setupFixturesForAssociationTest
  }

  def checkFieldFrom( mgr: BaseRecordManager[_])( javaName: String, 
                                                  colName: String,
                                                  how: MapAs.How
                                                ) = 
  {
    javaName should not equal (null)    // sanity-check args
    colName  should not equal (null)

    val fieldOpt = 
      mgr.fields.find{ f => 
        f.recordFieldName == javaName && 
        f.dbColumnName    == colName }

    fieldOpt should not equal (None)
    fieldOpt.map { _.mappedHow should equal (how) }
  }

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

  describe( "read-only mappings" ) {

    def fetchIt = {
      (TodoItemsSafed.whereEq("description"->"wash dog").fetchOnThisThread)(0)
    }

    // primary key is read-only, so we just use that.

    it ("should be fetched on a read") {
      assert( !fetchIt.id.isNewRecord )
    }
    it ("should not be saved from a new record") {
      val pairs = TodoItemsSafed.dataPairs( new TodoItemSafed )
      pairs.filter{ _._1 == "_id" } should have size (0)
    }
    it ("should not be saved from a fetched record") {
      val pairs = TodoItemsSafed.dataPairs( new TodoItemSafed )
      pairs.filter{ _._1 == "_id" } should have size (0)
    }
  }

  describe( "write-once mappings" ) {

    def fetchIt = 
      (TodoItemsSafed.whereEq("description"->"wash dog").fetchOnThisThread)(0)

    // TodoListId is write-once here...

    it ("should be fetched on a read") {
      val list = fetchIt.todoListId.fetchOnThisThread
      list.name should equal ("dog list")
    }
    it ("*should* be saved from a new record") {
      val pairs = TodoItemsSafed.dataPairs( new TodoItemSafed )
      pairs.filter{ _._1 == "todo_list_id" } should have size (1)
    }
    it ("should not be saved from a fetched record") {
      val pairs = TodoItemsSafed.dataPairs( fetchIt )
      pairs.filter{ _._1 == "todo_list_id" } should have size (0)
    }
  }

  describe( "field mapping to column names from an empty query result" ) {

    val checkField = checkFieldFrom( TodoItemsSafed )_

    it ( "should automatically map matching fields" ) {
      checkField( "description", "description", MapAs.ReadWrite )
    }

    it ( "should respect overrides" ) {
      checkField( "todoListId", "todo_list_id", MapAs.WriteOnce )
    }

    it ( "should handle the primary key" ) {
      checkField( "id", "_id", MapAs.ReadOnly )
    }

  }

  describe( "field mapping to column names from a static class" ) {

    val checkField = checkFieldFrom( CallLogEntries )_

    it( "should automatically map matching fields" ) {
      checkField( "number",     Calls.NUMBER,      MapAs.ReadWrite )
      checkField( "cachedName", Calls.CACHED_NAME, MapAs.ReadWrite )
      checkField( "date",       Calls.DATE,        MapAs.ReadWrite )
    }
    it( "should support overrides" ) {
      checkField( "callType",    Calls.TYPE,       MapAs.ReadWrite )
      checkFieldFrom( ContactData.phones )( 
                  "dataVersion", "data_version",   MapAs.ReadOnly )
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
      checkField( "id", android.provider.BaseColumns._ID, MapAs.ReadOnly )
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

      val checkField = checkFieldFrom( ContactData.phones )_

      it( "should automatically map matching fields" ) {
        checkField( "number",    CommonDataKinds.Phone.NUMBER, MapAs.ReadWrite )
        checkField( "label",     "data3",                      MapAs.ReadWrite )
        checkField( "labelType", "data2",                      MapAs.ReadWrite )
        checkField( "contactId", "contact_id",                 MapAs.ReadWrite )
      }
      it( "should map the ID field, if present" ) {
        checkField( "id", android.provider.BaseColumns._ID, MapAs.ReadOnly )
      }
    }

    describe ("catchall") {

      val checkField = checkFieldFrom( ContactData.unknowns )_

      it( "should automatically map matching fields" ) {
        checkField( "mimetype",  "mimetype",   MapAs.ReadWrite )
        checkField( "contactId", "contact_id", MapAs.ReadWrite )
      }
      it( "should map the ID field, if present" ) {
        checkField( "id", android.provider.BaseColumns._ID, MapAs.ReadOnly )
      }
    }
  }
}
