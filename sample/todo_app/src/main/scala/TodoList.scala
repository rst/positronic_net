package org.positronicnet.sample.todo

import org.positronicnet.db.Database
import org.positronicnet.util.WorkerThread

import org.positronicnet.orm._

// Our domain model classes, such as they are:  Todo Items, Lists, etc.
// Start by defining the DB schema...

object TodoDb 
 extends Database( filename = "todos.sqlite3", logTag = "todo" ) 
 with WorkerThread
{
  // This gets fed to a SQLiteOpenHelper, which implements the following
  // default behavior (unless overridden, of course):
  //
  // "version" is the length of schemaUpdates.
  // "onUpdate" runs all the updates from oldVersion to newVersion.
  // "onCreate" just runs 'em all.

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

// "Todo item" model.
//
// SoftDelete support, requiring the "is_deleted" column, is part
// of the library, but deals with the rest of it only through public
// interfaces; the intent is that similar things (multiple version
// hacks, to support more general undo, and so forth) be doable as
// extensions.

case class TodoItem( todoListId: Long    = ManagedRecord.unsavedId,
                     description: String = null, 
                     isDone: Boolean     = false,
                     id: Long            = ManagedRecord.unsavedId 
                   )
  extends ManagedRecord( TodoItems )
{
  def setDescription( s: String ) = copy( description = s )
  def setDone( b: Boolean )       = copy( isDone = b )
}

object TodoItems extends RecordManager[ TodoItem ]( TodoDb( "todo_items" ))
  with SoftDelete[ TodoItem ]

// "Todo list" model.  

case class TodoList( name: String = null,
                     id: Long     = ManagedRecord.unsavedId
                   )
  extends ManagedRecord( TodoLists )
{
  def setName( s: String ) = copy( name = s )

  // XXX parameterized query support

  lazy val items = new HasMany( TodoItems ) with SoftDeleteScope[ TodoItem ]
  lazy val doneItems = items.whereEq( "is_done" -> true )
}

object TodoLists extends RecordManager[ TodoList ]( TodoDb("todo_lists") )
  with SoftDelete[ TodoList ]

