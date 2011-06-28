package rst.todo

import org.triplesec.Button
import org.triplesec.IndexedSeqAdapter
import org.triplesec.Dialog
import org.triplesec.EditText
import org.triplesec.TextView
import org.triplesec.Activity
import org.triplesec.ListView
import org.triplesec.db.DatabaseWithThread

import _root_.android.os.Bundle
import _root_.android.content.Context
import _root_.android.content.Intent
import _root_.android.util.AttributeSet
import _root_.android.util.Log
import _root_.android.view.KeyEvent
import _root_.android.view.View
import _root_.android.graphics.Paint
import _root_.android.graphics.Canvas

import scala.collection.mutable.ArrayBuffer

// Getting sub-widgets, using the typed resources consed up by the
// android SBT plugin.  It would be nice to put this in a library,
// but the sbt-android plugin puts TypedResource itself in the app's
// main package --- so the library would have to import it from a
// different package in every app!

trait ViewFinder {
  def findView[T](  tr: TypedResource[T] ) = 
    findViewById( tr.id ).asInstanceOf[T]

  def findViewById( id: Int ): android.view.View
}

// Our domain model classes, such as they are.

object TodoDb 
extends DatabaseWithThread( filename = "todos.sqlite3", logTag = "todo" ) {

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

// Semi-formal domain model:

trait TodoDbModel {
  var changeHandler: (() => Unit) = null
  def onChange( handler: => Unit ) = { changeHandler = (() => handler) }
  def noteChange = { if (changeHandler != null) changeHandler() }
  def doChange( f: => Unit ) = { TodoDb.runOnDbThread{ f; noteChange } }
}

case class TodoItem( val id: Long, var description: String, var isDone: Boolean)

case class TodoList( val id: Long,
                     var name: String, 
                     val items: ArrayBuffer[TodoItem] = 
                       new ArrayBuffer[TodoItem])
extends TodoDbModel
{
  val dbItems = TodoDb( "todo_items" ).whereEq( "todo_list_id" -> this.id )

  def refreshFromDb = {
    doChange {
      items.clear
      for (c <- dbItems.order("id asc").select("id", "description", "is_done")){
        items += TodoItem( c.getLong(0), c.getString(1), c.getBoolean(2) )
      }
    }
  }

  def addItem( description: String, isDone: Boolean = false ) = {
    doChange {
      val id = TodoDb( "todo_items" ).insert( 
        "todo_list_id" -> this.id, 
        "description"  -> description,
        "is_done"      -> isDone )
      items += new TodoItem( id, description, isDone )
    }
  }

  def setItemDescription( posn: Int, desc: String ) = {
    doChange {
      val it = items(posn)
      dbItems.whereEq( "id"->it.id ).update("description"->desc)
      it.description = desc
    }
  }

  def setItemDone( posn: Int, isDone: Boolean ) = {
    doChange {
      val it = items(posn)
      dbItems.whereEq( "id"->it.id ).update("is_done" -> isDone)
      it.isDone = isDone
    }
  }

  def removeItem( posn: Int ) = {
    doChange {
      dbItems.whereEq( "id" -> items(posn).id ).delete
      items.remove( posn )
    }
  }

}

object Todo extends TodoDbModel {

  val lists = new ArrayBuffer[ TodoList ]
  val listNumKey = "listNum"            // For intents; see below.

  def refreshFromDb = {
    doChange {
      lists.clear
      for( c <- TodoDb("todo_lists").order("id asc").select( "id", "name" )) {
        lists += TodoList( c.getLong(0), c.getString(1) )
      }
    }
  }

  def addList( name: String ) = {
    doChange {
      val id = TodoDb( "todo_lists" ).insert( "name" -> name )
      lists += new TodoList( id, name )
    }
  }

  def removeList( posn: Int ) = {
    doChange {
      val list_id = lists(posn).id
      TodoDb( "todo_items" ).whereEq( "todo_list_id" -> list_id ).delete
      TodoDb( "todo_lists" ).whereEq( "id" -> list_id ).delete
      lists.remove( posn )
    }
  }
} 

// UI to deal with them, starting with an activity to manage the
// set of available todo lists.

class TodosActivity 
 extends Activity( layoutResourceId = R.layout.all_todos ) with ViewFinder {

  onCreate {
    val adapter = new TodosAdapter
    val listsView = findView( TR.listsView )

    listsView.setAdapter( adapter )
    Todo.onChange { this.runOnUiThread { ()=>{ adapter.notifyDataSetChanged }}}

    TodoDb.openInContext( getApplicationContext )
    Todo.refreshFromDb                  // Ideally, should be backgrounded

    listsView.onItemClick { (view, posn, id) => viewList( posn ) }
    listsView.onItemLongClick { (view, posn, id) => 
      new KillListDialog( this, posn ).show }

    findView( TR.addButton ).onClick { doAdd }
    findView( TR.newListName ).onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }
  }

  onDestroy { TodoDb.close }

  override def recreateInstanceState( b: Bundle ) {
    // No state to pull out of a bundle when recreating an instance,
    // but if there were, this is where we'd put the code to do it.
    // (The general philosophy I'm trying to follow here is that if
    // you always need to do something, like invoking "super.onCreate",
    // the framework ought to do it for you --- and if you might not
    // need to do something, like dealing with the Bundle, it shouldn't
    // be in your face.)
  }

  def doAdd = {
    val str = findView( TR.newListName ).getText.toString
    if ( str != "" ) {
      Todo.addList( name = str )
      findView( TR.newListName ).setText("")
    }
  }

  def viewList( posn: Int ) {
    val intent = new Intent( this, classOf[TodoActivity] )
    intent.putExtra( Todo.listNumKey, posn )
    startActivity( intent )
  }

  def removeList( posn: Int ) = {
    Todo.removeList( posn )
  }

}

// Its helper dialog.  I'm being really aggressive in trying to minimize
// the ceremony here, with consequences --- this code builds and throws
// away a new KillListDialog every time it needs one.  It would be more
// efficient to instantiate the thing once, and keep it around, for which
// see ItemEditDialog below.  (It's also more code, but not a lot.)

class KillListDialog( base: TodosActivity, victimPosn: Int ) 
 extends Dialog( base, layoutResourceId = R.layout.kill_todo_list ) 
 with ViewFinder 
{
  findView( TR.victimText ).setText("Delete "+Todo.lists(victimPosn).name+"?")
  findView( TR.deleteButton ).onClick{ base.removeList( victimPosn ); dismiss }
  findView( TR.cancelButton ).onClick{ dismiss }
}

class TodosAdapter
extends IndexedSeqAdapter( Todo.lists, itemViewResourceId = R.layout.todos_row){

  // getView inflates the itemViewResourceId from above, if it doesn't have
  // an old "convertView" supplied, and then calls this:

  override def fillView( view: View, position: Int ) = {
    view.asInstanceOf[ TextView ].setText( getItem( position ).name )
  }

  // If you want to do something else when creating the views besides just
  // inflating the layout resource, like populating a ViewHolder, there's
  // also a "createView" method that you can override.  But if you don't
  // need to do that, as when the rows are simple TextViews, the above is
  // all it takes.
}

// And now, the other activity, which manages an individual todo list.

class TodoActivity 
extends Activity( layoutResourceId = R.layout.todo_one_list) with ViewFinder {

  var theList: TodoList = null

  lazy val editDialog = new EditItemDialog(this,theList) // constructed once
  lazy val newItemText = findView( TR.newItemText )

  onCreate{

    // Setup

    theList = Todo.lists( getIntent.getIntExtra( Todo.listNumKey, -1 ))
    setTitle( "Todo for: " + theList.name )

    val adapter = new TodoItemsAdapter( theList.items )
    val listItemsView = findView( TR.listItemsView )
    listItemsView.setAdapter( adapter )
    theList.onChange { this.runOnUiThread { () => adapter.notifyDataSetChanged}}

    TodoDb.openInContext( getApplicationContext )
    theList.refreshFromDb

    // Event handlers...

    listItemsView.onItemClick { (view, posn, id) => toggleDone( posn ) }
    listItemsView.onItemLongClick { (view, posn, id) => editDialog.doEdit(posn)}
    findView( TR.addButton ).onClick { doAdd }
    newItemText.onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }
  }

  onDestroy { TodoDb.close }

  def doAdd = {
    val str = newItemText.getText.toString
    if ( str != "" ) {
      theList.addItem( description = str, isDone = false )
      newItemText.setText("")
    }
  }

  def setItemDescription( posn: Int, desc: String ) = {
    theList.setItemDescription( posn, desc )
  }

  def toggleDone( posn: Int ) = {
    theList.setItemDone( posn, !theList.items( posn ).isDone )
  }

  def removeItem( posn: Int ) = {
    theList.removeItem( posn )
  }
}

// Another dialog.  This one's meant to stick around; the "doEdit"
// method does setup and pops it up.

class EditItemDialog( base: TodoActivity, theList: TodoList )
extends Dialog( base, layoutResourceId = R.layout.dialog ) with ViewFinder {

  val editTxt = findView( TR.dialogEditText )
  var editingPosn = -1

  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }

  findView( TR.saveButton ).onClick { doSave; dismiss }
  findView( TR.deleteButton ).onClick { doDelete; dismiss }
  
  def doSave = {
    base.setItemDescription( editingPosn, editTxt.getText.toString )
  }

  def doDelete = { base.removeItem( editingPosn ) }
    
  def doEdit( posn: Int ) = {
    editingPosn = posn
    editTxt.setText( theList.items( posn ).description )
    show()
  }
  
}

// Another trivial adapter...

class TodoItemsAdapter(seq: IndexedSeq[TodoItem]) 
 extends IndexedSeqAdapter( seq, itemViewResourceId = R.layout.todo_row ) {

  override def fillView( view: View, position: Int ) = {
    view.asInstanceOf[ TodoItemView ].setTodoItem( getItem( position ))
  }
}

// And a modified TextView which modifies the text style based on
// whether the item "isDone" (adding strikethrough)...

class TodoItemView( context: Context, attrs: AttributeSet = null )
 extends TextView( context, attrs ) {
   def setTodoItem( item: TodoItem ) = {
     setText( item.description )
     setPaintFlags( 
       if (item.isDone) getPaintFlags | Paint.STRIKE_THRU_TEXT_FLAG 
       else getPaintFlags & ~Paint.STRIKE_THRU_TEXT_FLAG
     )
   }
}


