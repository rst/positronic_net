package rst.todo

import org.positronic.db.Database
import org.positronic.util.ChangeNotifications
import org.positronic.util.WorkerThread

import scala.collection.mutable.ArrayBuffer

// Our domain model classes, such as they are.

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

  // Since it's a DatabaseWithThread, it supports runOnDbThread,
  // and *requires* all database access (anything that calls
  // getReadableDatabase or getWritableDatabase) to be on that 
  // thread.

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
         """)
  
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
  val dbItems = TodoDb( "todo_items" ).whereEq( "todo_list_id" -> this.id )

  def refreshFromDb = {
    items.clear
    db { 
      for (c <- dbItems.order("id asc").select("id", "description", "is_done")){
        items += TodoItem( c.getLong(0), c.getString(1), c.getBoolean(2) )
      }
      noteChange // on DB Thread --- AFTER we've finished the query!
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

  def removeItem( posn: Int ) = {

    items.remove( posn )
    noteChange

    db { dbItems.whereEq( "id" -> items(posn).id ).delete }
  }

}

object Todo extends TodoDbModel {

  val lists = new ArrayBuffer[ TodoList ]

  def refreshFromDb = {
    lists.clear
    db {
      for( c <- TodoDb("todo_lists").order("id asc").select( "id", "name" )) {
        lists += TodoList( c.getLong(0), c.getString(1) )
      }
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

    val list_id = lists(posn).id
    lists.remove( posn )
    noteChange

    db {
      TodoDb( "todo_items" ).whereEq( "todo_list_id" -> list_id ).delete
      TodoDb( "todo_lists" ).whereEq( "id" -> list_id ).delete
    }
  }
} 

