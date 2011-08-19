package org.positronicnet.test

import org.positronicnet.db._
import org.positronicnet.orm._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class SingleThreadOrmSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  def haveItem[T<:Seq[TodoItem]]( description: String, 
                                  isDone: Boolean, 
                                  items: T ) =
    items.exists{ it => it.description == description && it.isDone == isDone }

  describe( "Single-thread ORM queries" ){
    it ("should find all the records") {
      val results = TodoItems.fetchOnThisThread
      results should have size (3)

      assert( haveItem( "wash dog" , false, results ))
      assert( haveItem( "feed dog" , false, results ))
      assert( haveItem( "walk dog" , true,  results ))
    }
    it ("should retrieve only matching records with conds"){
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread

      undoneItems should have size (2)
      assert( haveItem( "wash dog" , false, undoneItems ))
      assert( haveItem( "feed dog" , false, undoneItems ))
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
      TodoItems.whereEq( "is_done" -> true ).onThisThread( DeleteAll(TodoItem()) )
      TodoItems.count.value should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM delete via record selection" ) {
    it ("should kill the selected record") {
      val doneItems = TodoItems.whereEq( "is_done" -> true ).fetchOnThisThread
      doneItems should have size (1)

      doneItems.foreach{ TodoItems ! Delete( _ ) }

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
  }
}
