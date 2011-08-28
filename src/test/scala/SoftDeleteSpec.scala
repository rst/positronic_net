package org.positronicnet.test

import org.positronicnet.db._
import org.positronicnet.orm._
import org.positronicnet.content.ContentQuery

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.xtremelabs.robolectric.Robolectric

object TodoDbSD extends TodoDatabase( "todos_softdel.sqlite3" )
{
  // Variant schema, with "is_deleted" columns, still in the H2 dialect.
  //
  // Also, we set up with the dog todo-list itself soft-deleted,
  // and a few soft-deleted items on the cat todo-list.

  override def schemaUpdates =
    List(""" create table todo_lists (
               _id int identity,
               name varchar(100),
               is_deleted integer default 0
             )
         """,
         """ create table todo_items (
               _id int identity,
               todo_list_id int not null default -1,
               description varchar(100),
               is_done integer,
               is_deleted integer default 0
             )
         """)

  def setupFixturesForSoftDeleteTest = {

    val (dogListId, catListId) = setupFixturesForAssociationTest
    
    this("todo_lists").whereEq("_id" -> dogListId).update("is_deleted" -> true)

    this("todo_items").insert( "description"  -> "take cat to vet",
                               "todo_list_id" -> catListId,
                               "is_done"      -> true,
                               "is_deleted"   -> true )

    this("todo_items").insert( "description"  -> "hide catnip",
                               "todo_list_id" -> catListId,
                               "is_done"      -> true,
                               "is_deleted"   -> true )
  }
}

// Variants on TodoItem and TodoList for soft-delete tests.
// Necessary to get soft-delete record managers for 'em.
// Note that the "is_deleted" column is *not* mapped; this
// is intended usage.  (Soft-deleted items are ordinarily
// hidden from app code entirely, so it would be "false"
// any time you could look.)

case class TodoItemSD( description: String  = null, 
                       isDone:      Boolean = false,
                       todoListId:  Long    = ManagedRecord.unsavedId,
                       id:          Long    = ManagedRecord.unsavedId
                     )
  extends ManagedRecord( TodoItemsSD )
{
  def isDone( newVal: Boolean ) = copy( isDone = newVal )
}

object TodoItemsSD 
  extends RecordManager[ TodoItemSD ]( TodoDbSD("todo_items") )
  with SoftDelete[ TodoItemSD ]

case class TodoListSD( name: String = null,
                       id:   Long   = ManagedRecord.unsavedId )
  extends ManagedRecord( TodoListsSD )
{
  def name( newName: String ) = copy( name = newName )

  lazy val items = new HasMany( TodoItemsSD, "todo_list_id" )
                   with SoftDeleteQueries[ TodoItemSD ]
}

object TodoListsSD
  extends RecordManager[ TodoListSD ]( TodoDbSD("todo_lists") )
  with SoftDelete[ TodoListSD ]

// And now, the actual spec.

class SoftDeleteSpec
  extends Spec
  with ShouldMatchers    with BeforeAndAfterEach
  with RobolectricTests  with CommonDbTestHelpers
{
  describe( "soft deleted records" ) {

    it( "should not show on counts" ) {
      TodoListsSD.count.fetchOnThisThread   should equal (1)
      catList.items.count.fetchOnThisThread should equal (1)
    }

    it( "should not show on queries" ) {
      TodoListsSD.fetchOnThisThread.map{_.name}.toSeq should equal (
        Seq("cat list"))
      catList.items.fetchOnThisThread.map{_.description}.toSeq should equal(
        Seq("feed cat"))
    }
  }

  describe( "deletion with soft delete" ) {

    // Have to go to DB to see the is_deleted items...

    def catItemsRaw =
      db("todo_items").whereEq( "todo_list_id" -> catList.id)

    def catItemRawDescs =
      catItemsRaw.select( "description ").map{ _.getString(0) }

    it ("should expunge previously deleted items") {

      catItemsRaw.count should equal (3)

      catList.items.onThisThread( DeleteAll( TodoItemSD() ))
      catItemsRaw.count should equal (1)
    }

    it ("should make deleted items invisible") {
      catList.items.onThisThread( DeleteAll( TodoItemSD() ))
      catList.items.count.fetchOnThisThread should equal (0)
    }

    it ("should mark visible items is_deleted") {
      catList.items.onThisThread( DeleteAll( TodoItemSD() ))
      catItemsRaw.whereEq( "is_deleted" -> true ).count should equal (1)
      catItemRawDescs.toSeq should equal (Seq("feed cat"))
    }
  }

  describe( "undeletion" ) {

    it ("should update counts") {
      catList.items.onThisThread( Undelete( TodoItemSD() ))
      catList.items.count.fetchOnThisThread should equal (3)
    }

    it ("should make deleted items visible") {
      catList.items.onThisThread( Undelete( TodoItemSD() ))
      val descs = catList.items.fetchOnThisThread.map{ _.description }.sorted
      descs should equal (Seq("feed cat", "hide catnip", "take cat to vet"))
    }
  }

  describe( "soft delete queries" ){
    it ("should yield proper counts") {
      catList.items.numDeleted.fetchOnThisThread should equal (2)
      TodoListsSD.numDeleted.fetchOnThisThread should equal (1)
      catList.items.onThisThread( Undelete( TodoItemSD() ))
    }
    it ("should yield correct booleans") {
      catList.items.hasDeleted.fetchOnThisThread should equal (true)
      TodoListsSD.hasDeleted.fetchOnThisThread should equal (true)

      catList.items.onThisThread( Undelete( TodoItemSD() ))
      catList.items.hasDeleted.fetchOnThisThread should equal (false)
    }
  }

  lazy val db = {
    TodoDbSD.openInContext( Robolectric.application )
    TodoDbSD
  }

  var catList: TodoListSD = null

  override def beforeEach = {
    db.setupFixturesForSoftDeleteTest
    catList = TodoListsSD.whereEq("name"->"cat list").fetchOnThisThread(0)
  }

}