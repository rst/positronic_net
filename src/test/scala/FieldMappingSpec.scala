package org.positronicnet.orm.test

import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.test.{DbTestFixtures,TodoDb}

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
import android.provider.CallLog.Calls

object Resolver extends PositronicContentResolver( "call_log_dummy" )

case class CallLogEntry( callType:   Int    = 0, 
                         number:     String = null, 
                         cachedName: String = null, 
                         date:       Long   = 0,
                         id: RecordId[CallLogEntry] = CallLogEntries.unsavedId )
  extends ManagedRecord

object CallLogEntries
  extends RecordManagerForFields[ CallLogEntry, Calls ]( Resolver( Calls.CONTENT_URI ))
{
  mapField( "callType", Calls.TYPE )    // override to avoid reserved word
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
  }
}
