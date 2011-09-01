package org.positronicnet.orm.test

import org.positronicnet.db._
import org.positronicnet.test._
import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.orm.SoftDeleteActions._
import org.positronicnet.notifications.Actions._
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
  with ParentSoftDeleteListener[ TodoListSD ]
{
  // Various setup for parent listener stuff

  var lastParentOp: String            = null
  var lastParents:  Seq[ TodoListSD ] = null

  def onParentSoftDelete(qry: ContentQuery[_,_], scp: Scope[TodoListSD]):Unit={
    lastParentOp = "delete"
    lastParents = TodoListsSD.fetchRecords( qry )
  }

  def onParentUndelete(qry: ContentQuery[_,_], scope: Scope[TodoListSD]):Unit={
    lastParentOp = "undelete"
    lastParents = TodoListsSD.fetchRecords( qry )
  }
}

case class TodoListSD( name: String = null,
                       id:   Long   = ManagedRecord.unsavedId )
  extends ManagedRecord( TodoListsSD )
{
  def name( newName: String ) = copy( name = newName )

  lazy val items = new HasMany( TodoItemsSD, "todo_list_id" )
                   with SoftDeleteScope[ TodoItemSD ]
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

      catList.items.onThisThread( DeleteAll )
      catItemsRaw.count should equal (1)
    }

    it ("should make deleted items invisible") {
      catList.items.onThisThread( DeleteAll )
      catList.items.count.fetchOnThisThread should equal (0)
    }

    it ("should mark visible items is_deleted") {
      catList.items.onThisThread( DeleteAll )
      catItemsRaw.whereEq( "is_deleted" -> true ).count should equal (1)
      catItemRawDescs.toSeq should equal (Seq("feed cat"))
    }
  }

  describe( "undeletion" ) {

    it ("should update counts") {
      catList.items.onThisThread( Undelete )
      catList.items.count.fetchOnThisThread should equal (3)
    }

    it ("should make deleted items visible") {
      catList.items.onThisThread( Undelete )
      val descs = catList.items.fetchOnThisThread.map{ _.description }.sorted
      descs should equal (Seq("feed cat", "hide catnip", "take cat to vet"))
    }
  }

  describe( "soft delete queries" ){
    it ("should yield proper counts") {
      catList.items.numDeleted.fetchOnThisThread should equal (2)
      TodoListsSD.numDeleted.fetchOnThisThread should equal (1)
      catList.items.onThisThread( Undelete )
    }
    it ("should yield correct booleans") {
      catList.items.hasDeleted.fetchOnThisThread should equal (true)
      TodoListsSD.hasDeleted.fetchOnThisThread should equal (true)

      catList.items.onThisThread( Undelete )
      catList.items.hasDeleted.fetchOnThisThread should equal (false)
    }
  }

  describe( "soft deletion in scoped collections" ) {

    // Variant setup... dogList is *not* deleted, but it has deleted items
    // on it...

    var dogList: TodoListSD = null

    def itemsQuery( l: TodoListSD ) = 
      db( "todo_items" ).whereEq( "todo_list_id" -> l.id )

    def setupForCrossDeletionTests = {

      TodoListsSD.onThisThread( Undelete )
      dogList = TodoListsSD.whereEq("name"->"dog list").fetchOnThisThread(0)

      dogList.items.whereEq("description"->"wash dog").onThisThread( DeleteAll )

      itemsQuery( catList ).whereEq("is_deleted" -> true).count should equal (2)
      itemsQuery( dogList ).whereEq("is_deleted" -> true).count should equal (1)
      itemsQuery( dogList ).count should equal (3)
    }

    def assertDoglistExpungeOk( dogItemDels: Int, dogItemUndels: Int ) = {
      itemsQuery( catList ).whereEq("is_deleted" -> true).count should equal (2)
      itemsQuery( dogList ).whereEq("is_deleted" -> true).count should equal (dogItemDels)
      itemsQuery( dogList ).count should equal (dogItemDels + dogItemUndels)
    }

    it( "should expunge only within a given scope for deleteAll" ) {
      setupForCrossDeletionTests
      dogList.items.onThisThread( DeleteAll )
      assertDoglistExpungeOk( 2, 0 )
    }

    it( "should expunge only within a given scope for record delete" ) {
      setupForCrossDeletionTests
      val items = dogList.items.whereEq("description" -> "feed dog").fetchOnThisThread
      dogList.items.onThisThread( Delete( items(0) ))
      assertDoglistExpungeOk( 1, 1 )
    }
    
    it( "should expunge only within a given scope for subscoped delete" ) {
      setupForCrossDeletionTests
      val items = dogList.items.whereEq("description" -> "feed dog").fetchOnThisThread
      dogList.items.whereEq( "_id" -> items(0).id ).onThisThread( DeleteAll )
      assertDoglistExpungeOk( 1, 1 )
    }
    
    describe( "soft-delete-on-parent listener" ) {

      it ("should do nothing when nobody's listening") {
        TodoItemsSD.parentSoftDeleteListenerOption should equal (None)
      }

      it ("should notify on soft delete") {
        TodoListsSD.onThisThread( Delete( catList ))
        TodoItemsSD.lastParentOp should equal ("delete")
        TodoItemsSD.lastParents.map{ _.name } should equal (Seq("cat list"))
      }

      it ("should notify on undelete") {
        TodoListsSD.onThisThread( Undelete )
        TodoItemsSD.lastParentOp should equal ("undelete")
        TodoItemsSD.lastParents.map{ _.name } should equal (Seq("dog list"))
      }
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
