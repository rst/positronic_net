package org.positronicnet.test

import org.positronicnet.db._
import org.positronicnet.orm._

import com.xtremelabs.robolectric.Robolectric
import org.scalatest._

// A simple test database...

object TodoDb 
  extends Database( filename = "todos.sqlite3", logTag = "todo" ) 
{
  // Note that this schema definition is for H2, the Robolectric DB engine,
  // which speaks a different dialect from SQLite...

  def schemaUpdates =
    List(""" create table todo_items (
               _id int identity,
               description varchar(100),
               is_done integer
             )
         """)

  // Code to reset DB to a known, interesting state.

  def setupFixtures = {
    TodoDb( "todo_items" ).delete
    TodoDb( "todo_items" ).insert( "description" -> "wash dog",
                                   "is_done"     -> false )
    TodoDb( "todo_items" ).insert( "description" -> "feed dog",
                                   "is_done"     -> false )
    TodoDb( "todo_items" ).insert( "description" -> "walk dog",
                                   "is_done"     -> true )
  }

}

// ORM mappings for it.

case class TodoItem( description: String  = null, 
                     isDone:      Boolean = false,
                     id:          Long    = ManagedRecord.unsavedId
                   )
  extends ManagedRecord( TodoItems )
{
  def isDone( newVal: Boolean ) = copy( isDone = newVal )
}

object TodoItems extends RecordManager[ TodoItem ]( TodoDb("todo_items") )

// Common setup for tests that are going to use this stuff.
//
// (Annoying detail:  RobolectricTests must get mixed in *after*
// BeforeAndAfterEach, or the "beforeEach" setup code runs without
// Robolectric's bytecode swizzling, and dies on a "Stub!" exception.)

trait DbTestFixtures 
  extends BeforeAndAfterEach
  with RobolectricTests
{
  lazy val db = {
    TodoDb.openInContext( Robolectric.application )
    TodoDb
  }

  override def beforeEach = db.setupFixtures
}
