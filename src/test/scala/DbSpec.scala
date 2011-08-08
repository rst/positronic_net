package org.positronicnet.test

import org.positronicnet.db._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.xtremelabs.robolectric.Robolectric

object TodoDb 
  extends Database( filename = "todos.sqlite3", logTag = "todo" ) 
{
  // Note that this schema definition is for H2, the Robolectric DB engine,
  // which speaks a different dialect from SQLite...

  def schemaUpdates =
    List(""" create table todo_items (
               _id int identity,
               todo_list_id integer,
               description varchar(100),
               is_done integer
             )
         """)
}

class DbSpecs 
  extends Spec 
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
{
  lazy val db = {
    TodoDb.openInContext( Robolectric.application )
    TodoDb
  }

  override def beforeEach = {
    db( "todo_items" ).delete
    db( "todo_items" ).insert( "description" -> "wash dog",
                                   "is_done"     -> false )
    db( "todo_items" ).insert( "description" -> "feed dog",
                                   "is_done"     -> false )
    db( "todo_items" ).insert( "description" -> "walk dog",
                                   "is_done"     -> true )
  }

  describe( "queries" ){
    it( "should retrieve all records with no conditions" ){
      val results = db( "todo_items" ).select("description")
      val descriptions = results.map{ _.getString(0) }
      descriptions should have size (3)
      descriptions should contain ("walk dog")
      descriptions should contain ("wash dog")
      descriptions should contain ("feed dog")
    }
    it ("should retrieve only matching records with conds"){
      val undoneItems = db( "todo_items" ).whereEq( "is_done" -> false )
      val descriptions = undoneItems.select("description").map{_.getString(0)}
      descriptions should have size (2)
      descriptions should contain ("feed dog")
      descriptions should contain ("wash dog")
    }
  }

  describe( "delete" ) {
    it( "should delete matching records" ){
      db( "todo_items" ).whereEq( "is_done" -> true ).delete
      val descriptions = 
        db( "todo_items" ).select( "description" ).map{ _.getString(0) }
      descriptions should have size (2)
      descriptions should contain ("feed dog")
      descriptions should contain ("wash dog")
    }
  }
  describe( "update" ) {
    it( "should change things" ){
      db("todo_items").whereEq("description"->"feed dog").update("is_done"->true)
      val undoneItems = db( "todo_items" ).whereEq( "is_done" -> false )
      val descriptions = undoneItems.select("description").map{_.getString(0)}
      descriptions should have size (1)
      descriptions should contain ("wash dog")
    }
  }
}
