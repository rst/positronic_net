package org.positronicnet.sample.todo

import android.util.Log
import android.content.Intent

import org.positronicnet.db.Database
import org.positronicnet.db.DbQuery

import org.positronicnet.orm._
import org.positronicnet.content.ContentQuery

import org.positronicnet.util.WorkerThread
import org.positronicnet.util.ChangeManager

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

//================================================================
// "Todo item" model.
//
// See below for the definition of SoftDelete, which is an extension
// to the ORM...

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

//================================================================
// "Todo list" model.  

case class TodoList( name: String = null,
                     id: Long     = ManagedRecord.unsavedId
                   )
  extends ManagedRecord( TodoLists )
{
  // XXX number deleted.
  // XXX parameterized query support
  // These may wind up going together...

  lazy val items = TodoItems.whereEq( "todo_list_id" -> this.id )
  lazy val doneItems = items.whereEq( "is_done" -> true )

  def newItem = TodoItem( this.id )
  def setName( s: String ) = copy( name = s )
}

object TodoLists extends RecordManager[ TodoList ]( TodoDb("todo_lists") )
  with SoftDelete[ TodoList ]

//================================================================
// We're using a "soft deletion" scheme pretty broadly, which plugs in
// like so.  (Note that the "is_deleted" column in a soft-delete table
// doesn't need to be mapped in core, and usually isn't.)

trait SoftDelete[ T <: ManagedRecord ]
  extends BaseRecordManager[ T ]
{
  protected override def queryForAll( qry: ContentQuery[_,_] ) =
    super.queryForAll( qry ).whereEq( "is_deleted" -> false )

  protected override def deleteAll( qry: ContentQuery[_,_] ): Unit = {
    super.queryForAll( qry ).whereEq( "is_deleted" -> true ).delete
    super.queryForAll( qry ).update( "is_deleted" -> true )
  }
}

case class Undelete[T <: ManagedRecord : ClassManifest ](dummy: T) 
  extends ScopedAction[T]
{
  def act( qry: ContentQuery[_,_], mgr: BaseRecordManager[T] ): Unit =
    qry.whereEq( "is_deleted" -> true ).update( "is_deleted" -> false )
}

