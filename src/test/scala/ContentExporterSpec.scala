package org.positronicnet.test

import org.positronicnet.content._

import android.net.Uri
import android.content.ContentValues

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.xtremelabs.robolectric.Robolectric

import scala.collection.mutable.ArrayBuffer

// Declaration of a ContentProvider to implement the TodoContract
// that we've declared as a Java class in that portion of the test
// fixtures.
//
// Definitions here still distressingly verbose; doubly so if you
// factor in the Contract.

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
  import TodoContract._      // make 'static' stuff visible w/o prefix

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

  // Test scaffolding for change notifications --- verify that we're notifying
  // on the right URIs with the content resolver by mocking out the routine
  // that does it.  (Which means no test coverage on that routine itself, but
  // it's a trivial one-liner...)

  private val notifiedUris = new ArrayBuffer[ Uri ]

  override def notifyChange( uri: Uri ) = notifiedUris += uri

  def captureNotifiedUris( body: => Unit ) = {
    notifiedUris.clear
    body
    notifiedUris.toSeq
  }
}

class ContentExporterSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  // Machinery for setting up and accessing fixtures...

  override def beforeEach = db.setupFixturesForAssociationTest

  def makeTodosProvider = new TodoProvider

  def dogListId = {
    val dogListQuery = TodoDb("todo_lists").whereEq("name"->"dog list")
    dogListQuery.select("_id").map{_.getLong(0)}(0)
  }

  def walkDogId = {
    val dogListQuery = TodoDb("todo_items").whereEq("description"->"walk dog")
    dogListQuery.select("_id").map{_.getLong(0)}(0)
  }

  // The spec proper...

  import TodoContract._

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

  // Test 'order' and conditions on one URI only; code paths the same
  // for any match.

  describe ("queries with ordering") {
    
    lazy val todos = makeTodosProvider

    // Sort something both ways, to verify that it's using the order parameter,
    // and not just getting things right by accident.

    it ("should get ascending order right") {
      val namesCursor = todos.query( TODO_LISTS_URI, Seq("name").toArray,
                                     null, null, "name asc" )
      val names = new PositronicCursor( namesCursor ).map{_.getString(0)}.toSeq
      names should be (Seq("cat list", "dog list"))
    }
    it ("should get descending order right") {
      val namesCursor = todos.query( TODO_LISTS_URI, Seq("name").toArray,
                                     null, null, "name desc" )
      val names = new PositronicCursor( namesCursor ).map{_.getString(0)}.toSeq
      names should be (Seq("dog list", "cat list"))
    }
  }

  describe ("queries with extra conditions") {

    lazy val todos = makeTodosProvider

    it ("should process extra conditions without parameters") {
      val uri = todoListItemsUri( dogListId )
      val descsCursor = todos.query( uri, Seq("description").toArray,
                                     "description > 'w'", null, null )
      val descs = new PositronicCursor( descsCursor ).map{_.getString(0)}.toSeq
      descs should (have size (2) 
                    and contain ("walk dog") and contain ("wash dog"))
    }

    it ("should process extra conditions with parameters") {
      val uri = todoListItemsUri( dogListId )
      val descsCursor = todos.query( uri, Seq("description").toArray,
                                     "description > ?", Array("w"), null )
      val descs = new PositronicCursor( descsCursor ).map{_.getString(0)}.toSeq
      descs should (have size (2) 
                    and contain ("walk dog") and contain ("wash dog"))
    }
  }

  // Specs for DML (insert/update/delete)

  describe ("inserts") {

    lazy val todos = makeTodosProvider

    // Regenerate contentValues each time; dogListId may change as
    // fixtures are reloaded after every test.

    def myContentValues = {
      val cv = new ContentValues
      cv.put( "todo_list_id", new java.lang.Long( dogListId ))
      cv.put( "description", "furminate dog" )
      cv
    }

    it ("should actually insert the record") {
      todos.insert( todoListItemsUri( dogListId ), myContentValues )
      val qry = TodoDb( "todo_items" ).whereEq( "todo_list_id" -> dogListId )
      val descs = qry.select( "description" ).map{ _.getString(0) }
      descs should contain ("furminate dog")
    }

    it ("should return a working URI") {

      val uri = todos.insert( todoListItemsUri( dogListId ), myContentValues )
      todos.getType( uri ) should be (rowContentType( TODO_ITEM_TYPE ))
      
      val descsCursor = todos.query( uri, Array("description"), null,null,null )
      val descs = new PositronicCursor( descsCursor ).map{ _.getString(0) }
      descs should (have size (1) and contain ("furminate dog"))
    }

    it ("should properly notify") {

      val nUris = todos.captureNotifiedUris {
        todos.insert( todoListItemsUri( dogListId ), myContentValues )
      }

      nUris should (have size (1) and contain (todoListItemsUri( dogListId )))
    }
  }

  describe ("updates") {

    lazy val todos = makeTodosProvider
    def myContentValues = {
      val cv = new ContentValues
      cv.put( "is_done", new java.lang.Integer(1) )
      cv
    }

    it ("should update the records") {
      todos.update( todoListItemsUri( dogListId ), myContentValues, null, null )
      val isDoneCursor = todos.query( todoListItemsUri( dogListId ),
                                      Array( "is_done" ), null, null, null )
      val isDoneVals = new PositronicCursor( isDoneCursor ).map{ _.getInt(0) }
      isDoneVals.toSeq should be (Seq(1,1,1))
    }
    
    it ("should return the number of updated rows") {
      val rv = todos.update( todoListItemsUri( dogListId ), myContentValues,
                             null, null )
      rv should be (3)
    }
    
    it ("should correctly notify") {
      val nUris = todos.captureNotifiedUris {
        todos.update( todoListItemsUri( dogListId ), myContentValues, null,null)
      }
      nUris should (have size (1) and contain (todoListItemsUri( dogListId )))
    }
  }

  describe ("deletes") {

    lazy val todos = makeTodosProvider

    it ("should delete the records") {
      todos.delete( todoListItemsUri( dogListId ), null, null )
      val isDoneCursor = todos.query( todoListItemsUri( dogListId ),
                                      Array( "is_done" ), null, null, null )
      val isDoneVals = new PositronicCursor( isDoneCursor ).map{ _.getInt(0) }
      isDoneVals should have size (0)
    }
    
    it ("should return the number of deleted rows") {
      val rv = todos.delete( todoListItemsUri( dogListId ), null, null )
      rv should be (3)
    }
    
    it ("should correctly notify") {
      val nUris = todos.captureNotifiedUris {
        todos.delete( todoListItemsUri( dogListId ), null, null )
      }
      nUris should (have size (1) and contain (todoListItemsUri( dogListId )))
    }
  }
}
