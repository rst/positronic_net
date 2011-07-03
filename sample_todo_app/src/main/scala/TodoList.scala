package org.positronicnet.sample.todo

import android.util.Log
import android.content.Intent

import org.positronicnet.db.Database
import org.positronicnet.db.PositronicCursor
import org.positronicnet.db.DbQuery

import org.positronicnet.util.ChangeNotifications
import org.positronicnet.util.WorkerThread

// Our domain model classes, such as they are:  Todo Items, Lists, etc.
// 
// NB operations on these affect the database, so they happen on a
// separate "db thread".  UI components can register as listeners for
// changes on domain objects, and if they do, they get fresh cursors
// with which to update themselves when things do change.  
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
               _id integer primary key,
               name string
             )
         """,
         """ create table todo_items (
               _id integer primary key,
               todo_list_id integer,
               description string,
               is_done integer
             )
         """,
         " alter table todo_lists add column is_deleted integer default 0 ",
         " alter table todo_items add column is_deleted integer default 0 "
        )
  
}

// Semi-formal domain model.  There's no ORM here, just an AREL-style
// gloss for building SQL, and a stylized way of building model classes
// that use it.  But it still cuts down on the clutter.
//
// BTW, a "PositronicCursor" is just a regular cursor with getBoolean
// added (implementing the sqlite convention that nonzero means
// "true").  Aside from that (and a 'foreach' method which iterates
// over the rows, which you can ignore unless you want to use it),
// they're just plain old Cursors.

trait TodoDbModel
  extends ChangeNotifications[ PositronicCursor ]
{
  // Wrapper for domain operations, which all hit the DB:
  // we run on the DB thread, and notify the listeners when done.

  def dbWrap( thunk: => Unit ) = { 
    TodoDb.runOnThread{ thunk; noteChangeEach{ requery }} 
  }

  protected def requery: PositronicCursor
}

// "Todo item" model.
// 
// Mostly actually manipulated from within TodoList; with a more
// complicated schema, it might be better to get these query fragments
// from methods invoked on the TodoItem companion object.

case class TodoItem(var id: Long, var description: String, var isDone: Boolean)

object TodoItem {

  def doQuery( query: DbQuery )= query.select("_id", "description", "is_done")

  def fromCursor( c: PositronicCursor ) = 
    TodoItem( c.getLong( 0 ), c.getString( 1 ), c.getBoolean( 2 ))

}

// "Todo list" model.  Serializable so it can be stuffed in intents.

case class TodoList( var id: Long, var name: String )
 extends TodoDbModel
{
  // In-core status fields; set on requery.

  var hasDoneItems = false
  var hasDeletedItems = false

  // Setting up (and use of) prebaked query fragments.

  private lazy val dbItemsAll = TodoDb("todo_items").whereEq("todo_list_id"->id)
  private lazy val dbItems    = dbItemsAll.whereEq( "is_deleted" -> false )

  // Method to get cursors for our change listeners (and refresh cached state):

  protected def requery: PositronicCursor = {
    hasDoneItems = dbItems.whereEq( "is_done" -> true ).count > 0
    hasDeletedItems = dbItemsAll.whereEq( "is_deleted" -> true ).count > 0
    return TodoItem.doQuery( dbItems )
  }

  // Public interface --- dealing with items.
  //
  // Note that we requery after every change, so we don't bother updating
  // the old in-core copies...

  def refreshFromDb = dbWrap {/* nothing */}

  def addItem( description: String, isDone: Boolean = false ) = dbWrap { 
    TodoDb( "todo_items" ).insert( 
        "todo_list_id" -> this.id, 
        "description"  -> description,
        "is_done"      -> isDone )
  }

  def setItemDescription( it: TodoItem, desc: String ) = dbWrap { 
    TodoDb("todo_items").whereEq("_id" -> it.id).update( "description" -> desc )
  }

  def setItemDone( it: TodoItem, isDone: Boolean ) = dbWrap { 
    TodoDb("todo_items").whereEq("_id" -> it.id).update( "is_done" -> isDone )
  }

  def deleteWhereDone = dbWrap {
    dbItemsAll.whereEq( "is_deleted" -> true ).delete // purge the last batch
    dbItems.whereEq( "is_done" -> true ).update( "is_deleted" -> true )
  }

  def undeleteItems = dbWrap { dbItemsAll.update( "is_deleted" -> false ) }

  // ... and the list itself

  def setName( newName: String ) = {
    name = newName; 
    TodoDb("todo_lists").whereEq("_id" -> this.id).update( "name" -> newName )
  }
}

object TodoList {

  def doQuery( query: DbQuery ) = query.select("_id", "name")

  def fromCursor( c: PositronicCursor ) = 
    TodoList( c.getLong( 0 ), c.getString( 1 ))

  def create( name: String ) = TodoDb( "todo_lists" ).insert( "name" -> name )

  // Communicating these through intents...
  // Sadly, this is easier than making them serializable.

  val intentIdKey = "todoListId"; val intentNameKey = "todoListName"

  def intoIntent( list: TodoList, intent: Intent ) = {
    intent.putExtra( intentIdKey,   list.id )
    intent.putExtra( intentNameKey, list.name )
  }

  def fromIntent( intent: Intent ) = 
    TodoList( intent.getLongExtra( intentIdKey, -1 ), 
              intent.getStringExtra( intentNameKey ))
}

// Singleton object to represent the set of all available lists.

object TodoLists extends TodoDbModel 
{
  private lazy val dbListsAll = TodoDb("todo_lists")
  private lazy val dbLists = dbListsAll.whereEq("is_deleted"-> false)

  var hasDeleted = false                // reset on query

  protected def requery: PositronicCursor = {
    hasDeleted = dbListsAll.whereEq( "is_deleted" -> true ).count > 0
    return TodoList.doQuery( dbLists )
  }

  // Public interface

  def refreshFromDb = dbWrap{}

  def addList( name: String ) = dbWrap { TodoList.create( name ) }

  def setListName( list: TodoList, newName: String ) = dbWrap {
    list.setName( newName )
  }

  def removeList( victim: TodoList ) = dbWrap {

    // Purge all previously deleted lists...
    for ( c <- dbListsAll.whereEq( "is_deleted" -> true ).select( "_id" )) {
      val purgedListId = c.getLong(0)
      TodoDb( "todo_items" ).whereEq( "todo_list_id" -> purgedListId ).delete
      TodoDb( "todo_lists" ).whereEq( "_id" -> purgedListId ).delete
    }

    // And mark this one for the axe...
    TodoDb("todo_lists").whereEq("_id"->victim.id).update("is_deleted" -> true)
  }

  def undelete = dbWrap { dbListsAll.update( "is_deleted" -> false ) }
} 
