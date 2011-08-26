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
  // which speaks a different dialect from SQLite.
  //
  // Note also the very dodgy treatment of the todo_lists/todo_items
  // association:
  //
  // We want to have tests which test and deal with associations.
  // We want to have other tests that work on the todo_items table,
  // and ignore the association.  The dodgy way we do this is to 
  // have the todo_list_id default to an invalid value.  That way,
  // we can write a setupFixtures method that sets up lists and 
  // associated items, and another that stuffs junk into the
  // todo_list_id foreign key and ignores the association 
  // completely.

  def schemaUpdates =
    List(""" create table todo_lists (
               _id int identity,
               name varchar(100)
             )
         """,
         """ create table todo_items (
               _id int identity,
               todo_list_id int not null default -1,
               description varchar(100),
               is_done integer
             )
         """)

  // Code to reset DB to a known, interesting state.

  def setupFixturesForSimpleTest = {
    TodoDb( "todo_lists" ).delete
    TodoDb( "todo_items" ).delete
    setupDogItems( -1 )
  }

  def setupFixturesForAssociationTest = {

    TodoDb( "todo_lists" ).delete
    TodoDb( "todo_items" ).delete

    val listId = TodoDb( "todo_lists" ).insert( "name" -> "dog list" )
    setupDogItems( listId )

    val otherListId = TodoDb( "todo_lists" ).insert( "name" -> "cat list" )
    TodoDb( "todo_items" ).insert( "description"  -> "feed cat",
                                   "todo_list_id" -> otherListId,
                                   "is_done"      -> false )

  }

  private def setupDogItems( listId: Long ) = {
    TodoDb( "todo_items" ).insert( "description"  -> "wash dog",
                                   "todo_list_id" -> listId,
                                   "is_done"      -> false )
    TodoDb( "todo_items" ).insert( "description"  -> "feed dog",
                                   "todo_list_id" -> listId,
                                   "is_done"      -> false )
    TodoDb( "todo_items" ).insert( "description"  -> "walk dog",
                                   "todo_list_id" -> listId,
                                   "is_done"      -> true )
  }
}

// ORM mappings for it.

case class TodoItem( description: String  = null, 
                     isDone:      Boolean = false,
                     todoListId:  Long    = ManagedRecord.unsavedId,
                     id:          Long    = ManagedRecord.unsavedId
                   )
  extends ManagedRecord( TodoItems )
{
  def isDone( newVal: Boolean ) = copy( isDone = newVal )
}

object TodoItems extends RecordManager[ TodoItem ]( TodoDb("todo_items") )

case class TodoList( name: String = null,
                     id:   Long   = ManagedRecord.unsavedId )
  extends ManagedRecord( TodoLists )
{
  lazy val items = new HasMany( TodoItems, "todo_list_id", id ) 

  def name( newName: String ) = copy( name = newName )
}

object TodoLists extends RecordManager[ TodoList ]( TodoDb("todo_lists") )

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

  override def beforeEach = db.setupFixturesForSimpleTest

  // Only useful for ORM tests, but harmless elsewhere...

  def haveItem[T<:Seq[TodoItem]]( description: String, 
                                  isDone: Boolean, 
                                  items: T ) =
    items.exists{ it => it.description == description && it.isDone == isDone }
}
