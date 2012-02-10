package org.positronicnet.test

import org.positronicnet.db._
import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.notifications.Actions._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class AssociationSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  var dogList: TodoList = null
  var catList: TodoList = null

  override def beforeEach = {
    db.setupFixturesForAssociationTest
    val lseq = TodoLists.fetchOnThisThread
    dogList = lseq.find{ _.name == "dog list" }.get
    catList = lseq.find{ _.name == "cat list" }.get
  }

  describe( "record ID as belongs to association" ) {
    it( "should fetch correct values" ) {

      for( item <- dogList.items.fetchOnThisThread ) 
        item.todoListId.fetchOnThisThread should equal (dogList)

      for( item <- catList.items.fetchOnThisThread ) 
        item.todoListId.fetchOnThisThread should equal (catList)
    }
  }

  describe( "has many association" ) {

    it( "should find only relevant records" ) {
      TodoItems.count.fetchOnThisThread should equal(4)
      dogList.items.count.fetchOnThisThread should equal(3)
      assert( dogList.items.fetchOnThisThread.forall { item => 
          item.todoListId == dogList.id })
    }

    it( "should propagate change notifications across copies" ) {

      // Set up two copies of a TodoList, with two distinct 'items'
      // association proxies.  Monitor count on one, update the other,
      // and verify that the change propagates appropriately.

      val dogListCopy = dogList.copy()
      var monitoredCount: Long = -457     // clearly invalid...

      dogList.items.count.watch( this ){ count => { monitoredCount = count }}
      
      dogListCopy.items.onThisThread( DeleteAll )

      monitoredCount should equal (0)

      dogList.items.count.stopNotifier( this )
    }

    it ( "should have a working create()" ) {
      dogList.items.create.todoListId should equal (dogList.id)
    }

    it ( "should delete dependent records" ) {

      val dogListId = dogList.id
      val catListId = catList.id

      def itemCount( listId: RecordId[ TodoList ] ) = 
        TodoDb("todo_items").whereEq( "todo_list_id" -> listId.id ).count

      itemCount( dogListId ) should equal (3)
      itemCount( catListId ) should equal (1)

      TodoLists.onThisThread( Delete( dogList ))
      itemCount( dogListId ) should equal (0)
      itemCount( catListId ) should equal (1)
    }

    it ( "should note change when deleting dependent records" ) {

      var monitoredCount: Long = -472
      dogList.items.count.watch( this ){ count => { monitoredCount = count }}

      TodoLists.onThisThread( Delete( dogList ))
      monitoredCount should equal (0)

      dogList.items.count.stopNotifier( this )
    }
  }

  describe( "one-to-many join support" ) {
    it ("should fetch appropriate values, including outer join handling") {

      // This declaration is a mess, but adequate to model implicit
      // joins in content providers, which is most of what we want
      // this for right now.
      //
      // We'll probably want something later which computes the column
      // names and queries, assuming that we have simple mappings to tables
      // in the same DB.  But that's tricky, particularly if the base query
      // is to a *subset* of the rows in the underlying table (as for, e.g.,
      // soft delete).

      object myJoin  extends OneToManyJoin( 
        TodoLists, TodoItems,
        Seq("todo_lists._id", "name", 
            "todo_items._id", "todo_list_id", "description", "is_done"),
        TodoDb( "todo_lists left join todo_items on" +
                " todo_lists._id = todo_items.todo_list_id" ).
          order ("name, description")
      ) {
        remap( LeftCol("_id"),  "todo_lists._id" )
        remap( RightCol("_id"), "todo_items._id" )
      }

      // Toss in an empty list, for the outer join.

      TodoLists.onThisThread( Save (TodoList( "empty list" )))
      val emptyList = 
        (TodoLists.whereEq( "name" -> "empty list" ).fetchOnThisThread)(0)

      // Run a query, and see what happens...

      val theGoods = myJoin.fetchOnThisThread

      theGoods(0)._1 should equal (catList)
      theGoods(1)._1 should equal (dogList)
      theGoods(2)._1 should equal (emptyList)

      for ( (list, items) <- theGoods ) {
        items.toSeq should equal (
          list.items.order("description").fetchOnThisThread.toSeq)
      }
    }
  }
}
