package org.positronicnet.test

import org.positronicnet.content._

import android.net.Uri

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.xtremelabs.robolectric.Robolectric

// I guess in "real life" you'd want to make this a shim Java class
// with statics, for the sake of other apps on the stock technology
// stack.
//
// Definitions here still distressingly verbose.
//
// (BTW, when vals here are declared "lazy" for no obvious reason,
// it's usually because the code that supplies the value would hit
// a "Stub!" exception unless loaded through the Robolectric class
// loader.)

object TodoProvider {

  val TODO_PREFIX = "content://org.positronicnet.test/"
  val TODO_LISTS_PREFIX = TODO_PREFIX + "lists"
  lazy val TODO_LISTS_URI = Uri.parse( TODO_LISTS_PREFIX )

  def todoListUri( id: Long ) = 
    Uri.withAppendedPath( TODO_LISTS_URI, id.toString )
    
  lazy val TODO_LIST_ITEMS_PATTERN = TODO_LISTS_PREFIX + "/=/items"

  def todoListItemsUri( listId: Long ) =
    Uri.withAppendedPath( TODO_LISTS_URI, listId.toString + "/items" )

  val TODO_LIST_ITEM_PATTERN = TODO_LISTS_PREFIX + "/=/items/="

  def todoListItemUri( listId: Long, itemId: Long ) =
    Uri.withAppendedPath( TODO_LISTS_URI, listId.toString +"/items/"+ itemId )

  val TODO_LIST_TYPE = "vnd.org.positronicnet.todolist"
  val TODO_ITEM_TYPE = "vnd.org.positronicnet.todoitem"

  // Might want these somewhere where they'd be more generally useful

  def dirContentType(s: String) = "vnd.android.cursor.dir/"+s
  def rowContentType(s: String) = "vnd.android.cursor.item/"+s
}

class TodoProvider extends PositronicContentProvider
{
  import TodoProvider._             // make 'static' stuff visible w/o prefix

  def onCreate = { TodoDb.openInContext( getContext ); true }

  matchUriObj( TODO_LISTS_URI, dirContentType( TODO_LIST_TYPE )){
    noArgs => TodoDb("todo_lists")
  }
  matchUriStr( TODO_LISTS_PREFIX+"/=", rowContentType( TODO_LIST_TYPE )){
    seq => TodoDb("todo_lists").whereEq( "_id" -> seq(0) )
  }
  matchUriStr( TODO_LISTS_PREFIX+"/=/items", dirContentType( TODO_ITEM_TYPE )) {
    seq => TodoDb("todo_items").whereEq( "todo_list_id" -> seq(0) )
  }
  matchUriStr(TODO_LISTS_PREFIX+"/=/items/=", rowContentType( TODO_ITEM_TYPE )){
    seq => TodoDb("todo_items").whereEq("todo_list_id"->seq(0), "_id"->seq(1))
  }
}

class ContentExporterSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  override def beforeEach = db.setupFixturesForAssociationTest

  def makeTodosProvider = new TodoProvider

  import TodoProvider._

  describe( "content types" ) {

    it ("should yield correct values for single table") {
      val todos = makeTodosProvider
      todos.getType( TODO_LISTS_URI ) should be( dirContentType(TODO_LIST_TYPE))
      todos.getType( todoListUri(3) ) should be( rowContentType(TODO_LIST_TYPE))
    }

    it ("should yield correct values for nested table") {
      val todos = makeTodosProvider
      todos.getType( todoListItemsUri( 3 ) ) should be( 
        dirContentType(TODO_ITEM_TYPE))
      todos.getType( todoListItemUri( 3, 3 ) ) should be( 
        rowContentType(TODO_ITEM_TYPE))
    }
  }

  describe( "queries without conditions or order" ) {
    
    lazy val todos = makeTodosProvider

    def dogListId = {
      val dogListQuery = TodoDb("todo_lists").whereEq("name"->"dog list")
      dogListQuery.select("_id").map{_.getLong(0)}(0)
    }
    def walkDogId = {
      val dogListQuery = TodoDb("todo_items").whereEq("description"->"walk dog")
      dogListQuery.select("_id").map{_.getLong(0)}(0)
    }

    it ("should get dir for a simple table") {
      val namesCursor = todos.query( TODO_LISTS_URI, Seq("name").toArray,
                                     null, null, null )
      val names = new PositronicCursor( namesCursor ).map{ _.getString(0) }
      names should have size (2)
      names should (contain ("cat list") and contain ("dog list"))
    }
    it ("should get item from a simple table") {
      val namesCursor = todos.query( todoListUri(dogListId),Seq("name").toArray,
                                     null, null, null )
      val names = new PositronicCursor( namesCursor ).map{ _.getString(0) }
      names should (have size (1) and contain ("dog list"))
    }
    it ("should get dir from a nested query") {
      val descsCursor = 
        todos.query( todoListItemsUri( dogListId ), Seq("description").toArray,
                     null, null, null )
      val descs = new PositronicCursor( descsCursor ).map{ _.getString(0) }
      descs should (have size (3) 
                    and contain ("wash dog") 
                    and contain ("walk dog") 
                    and contain ("feed dog"))
    }
    it ("should get item from a nested query") {
      val descsCursor = 
        todos.query( todoListItemUri( dogListId, walkDogId ), 
                     Seq("description").toArray,
                     null, null, null )
      val descs = new PositronicCursor( descsCursor ).map{ _.getString(0) }
      descs should (have size (1) and contain ("walk dog"))
    }
    it ("should use all IDs in a nested query") {
      val descsCursor = 
        todos.query( todoListItemUri( dogListId + 1, walkDogId ), 
                     Seq("description").toArray,
                     null, null, null )
      val descs = new PositronicCursor( descsCursor ).map{ _.getString(0) }
      descs should (have size (0))
    }
  }
}
