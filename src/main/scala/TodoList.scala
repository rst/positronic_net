package rst.todo

import org.positronic.db.Database
import org.positronic.util.ChangeNotifications
import org.positronic.util.WorkerThread

import scala.collection.mutable.ArrayBuffer

// Our domain model classes, such as they are.
// 
// Note that these maintain an in-core copy of the lists in addition
// to what's in the database; for most UI operations, we operate on
// the in-core copy and update the display on the main thread, while
// kicking off an update on a DB worker thread to later (hopefully
// very soon!) mirror the change into the DB.  Which, in turn, keeps
// us from being in a situation where the main thread (and the UI) is
// waiting for the database.
//
// Note also that we're managing a "soft deletion" scheme here.
// User-level "delete" operations just set an "is_deleted" flag on the
// objects of the user's disaffection; they don't actually delete them
// immediately.  If the user has second thoughts, they can then
// "undelete" (resetting the flag) until the next batch of deletions,
// at which point the last batch really is purged.  This tends to be
// more effective than confirmation dialogs at helping users recover
// from mistakes.

object TodoDb 
 extends Database( filename = "todos.sqlite3", logTag = "todo" ) 
 with WorkerThread
{
  // This gets fed to a SQLiteOpenHelper, which implements the following
  // default behavior:
  //
  // "version" is the length of schemaUpdates.
  // "onUpdate" runs all the updates from oldVersion to newVersion.
  // "onCreate" just runs 'em all.
  //
  // This can all be overridden if appropriate (e.g., override
  // onCreate if running all updates serially is a silly way to create
  // a completely new database with the current schema).

  def schemaUpdates =
    List(""" create table todo_lists (
               id integer primary key,
               name string
             )
         """,
         """ create table todo_items (
               id integer primary key,
               todo_list_id integer,
               description string,
               is_done integer
             )
         """,
         " alter table todo_lists add column is_deleted integer default 0 ",
         " alter table todo_items add column is_deleted integer default 0 "
        )
  
}

// Semi-formal domain model.
//
// This version of the code maintains an in-core copy of the data,
// and a backing store in the above database.  On updates, the in-core
// copy is updated (and our changeHandler notified) on the UI thread, 
// and the database update is pushed onto the DB thread.  This way,
// the UI never has to wait for a database write to finish.
//
// (Well, almost never --- when we're first starting up, we obviously
// have to wait for the query to finish!)

trait TodoDbModel extends ChangeNotifications {
  def db( f: => Unit ) = { TodoDb.runOnThread{ f } }
}

case class TodoItem( var id: Long, var description: String, var isDone: Boolean)

case class TodoList( var id: Long,
                     var name: String, 
                     val items: ArrayBuffer[TodoItem] = 
                       new ArrayBuffer[TodoItem])
extends TodoDbModel
{
  val dbItemsAll = TodoDb( "todo_items" ).whereEq( "todo_list_id" -> this.id )
  val dbItems = dbItemsAll.whereEq( "is_deleted"   -> false )
  var hasDeletedItems = false

  def hasDoneItems = { items.indexWhere( it => it.isDone ) >= 0 }

  def refreshFromDb = {
    items.clear
    db { 
      for (c <- dbItems.order("id asc").select("id", "description", "is_done")){
        items += TodoItem( c.getLong(0), c.getString(1), c.getBoolean(2) )
      }
      hasDeletedItems = (dbItemsAll.whereEq( "is_deleted" -> true ).count > 0)
      noteChange
    }
  }

  def addItem( description: String, isDone: Boolean = false ) = {

    // Put the new item on our in-core list...

    val item = new TodoItem( -1, description, isDone )
    items += item
    noteChange

    // ... and have the DB thread find an ID for it.
    // (This means that the UI won't have access to the ID --- but
    // it's only subsequent DB ops that care, and those run in order,
    // so it'll be there when it's needed.)

    db {
      item.id = TodoDb( "todo_items" ).insert( 
        "todo_list_id" -> this.id, 
        "description"  -> description,
        "is_done"      -> isDone )
    }
  }

  def setItemDescription( posn: Int, desc: String ) = {

    val it = items(posn)
    it.description = desc
    noteChange

    db { dbItems.whereEq( "id"->it.id ).update("description"->desc) }
  }

  def setItemDone( posn: Int, isDone: Boolean ) = {

    val it = items(posn)
    it.isDone = isDone
    noteChange

    db { dbItems.whereEq( "id"->it.id ).update("is_done" -> isDone) }
  }

  def deleteWhereDone = {
    db {
      dbItemsAll.whereEq( "is_deleted" -> true ).delete // purge the last batch
      dbItems.whereEq( "is_done" -> true ).update( "is_deleted" -> true )
      refreshFromDb
    }
  }

  def undelete = {
    db {
      dbItemsAll.update( "is_deleted" -> false )
      refreshFromDb
    }
  }
}

object Todo extends TodoDbModel {

  val dbListsAll = TodoDb("todo_lists")
  val dbLists = dbListsAll.whereEq( "is_deleted" -> false )

  val lists = new ArrayBuffer[ TodoList ]
  var hasDeleted = false

  def refreshFromDb = {
    lists.clear
    db {
      for( c <- dbLists.order("id asc").select( "id", "name" )) {
        lists += TodoList( c.getLong(0), c.getString(1) )
      }
      hasDeleted = (dbListsAll.whereEq( "is_deleted" -> true ).count > 0)
      noteChange
    }
  }

  def addList( name: String ) = {

    val theList = new TodoList( -1, name )
    lists += theList
    noteChange

    db { theList.id = TodoDb( "todo_lists" ).insert( "name" -> name ) }
  }

  def removeList( posn: Int ) = {

    val victim = lists(posn)
    lists.remove( posn )
    hasDeleted = true
    noteChange

    db {

      // Purge all previously deleted lists...
      for ( c <- dbListsAll.whereEq( "is_deleted" -> true ).select( "id" )) {
        val purgedListId = c.getLong(0)
        TodoDb( "todo_items" ).whereEq( "todo_list_id" -> purgedListId ).delete
        TodoDb( "todo_lists" ).whereEq( "id" -> purgedListId ).delete
      }

      // And mark this one for the axe...
      dbLists.whereEq( "id" -> victim.id ).update( "is_deleted" -> true )
    }
  }

  def undelete {
    db {
      dbListsAll.update( "is_deleted" -> false )
      refreshFromDb
    }
  }
} 

