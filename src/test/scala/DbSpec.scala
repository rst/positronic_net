package org.positronicnet.test

import org.positronicnet.db._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.xtremelabs.robolectric.Robolectric

class DbSpecs 
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  describe( "simplequeries" ){
    it( "should retrieve all records with no conditions" ){
      val results = db( "todo_items" ).select("description")
      val descriptions = results.map{ _.getString(0) }
      descriptions should have size (3)
      descriptions should contain ("walk dog")
      descriptions should contain ("wash dog")
      descriptions should contain ("feed dog")
    }
  }
  describe( "whereEq" ) {
    it ("should retrieve only matching records"){
      val undoneItems = db( "todo_items" ).whereEq( "is_done" -> false )
      val descriptions = undoneItems.select("description").map{_.getString(0)}
      descriptions should have size (2)
      descriptions should contain ("feed dog")
      descriptions should contain ("wash dog")
    }
  }
  describe( "where" ) {
    it ("should retrieve only matching records"){
      val descPastG = db( "todo_items" ).where( "description > ?", "g" )
      val descriptions = descPastG.select("description").map{_.getString(0)}
      assert( descriptions.sorted == Seq("walk dog", "wash dog"))
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
