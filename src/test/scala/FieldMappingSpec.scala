package org.positronicnet.test

import org.positronicnet.orm._
import org.positronicnet.orm.Actions._

// Variant mapping for TodoItems which "wraps" description.
// (A stand-in for more useful types of transformations...)

case class WrappedString( raw: String )

case class TodoItemWithWrapping ( 
    rawDescription: String  = null, 
    isDone:         Boolean = false,
    id:             Long    = ManagedRecord.unsavedId
  )
  extends ManagedRecord( TodoItemsWithWrapping )
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
}
