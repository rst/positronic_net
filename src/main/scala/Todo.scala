package rst.todo

import org.triplesec.Button
import org.triplesec.IndexedSeqAdapter
import org.triplesec.Dialog
import org.triplesec.EditText
import org.triplesec.TextView
import org.triplesec.Activity
import org.triplesec.ListView

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

case class TodoItem( var description: String, var isDone: Boolean )
case class TodoList( var name: String, 
                     val items: ArrayBuffer[TodoItem] = 
                       new ArrayBuffer[TodoItem])

object Todo {
  val lists = new ArrayBuffer[ TodoList ]
  val listNumKey = "listNum"
} 

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

class TodoAdapter(seq: IndexedSeq[TodoItem]) 
extends IndexedSeqAdapter( seq, itemViewResourceId = R.layout.todo_row ) {

  override def fillView( view: View, position: Int ) = {
    view.asInstanceOf[ TodoItemView ].setTodoItem( getItem( position ))
  }
}

class EditDialog( base: TodoActivity, 
                  todos: ArrayBuffer[TodoItem] ) 
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
    editTxt.setText( todos(posn).description )
    show()
  }
  
}

class TodoActivity 
extends Activity( layoutResourceId = R.layout.todo_one_list) with ViewFinder {

  var todoItems: ArrayBuffer[TodoItem] = null
  var adapter: TodoAdapter = null

  lazy val editDialog = new EditDialog( this, todoItems )
  lazy val listItemsView = findView( TR.listItemsView )
  lazy val newItemText = findView( TR.newItemText )

  onCreate{

    // Setup

    val theList = Todo.lists( getIntent.getIntExtra( Todo.listNumKey, -1 ))

    setTitle( "Todo for: " + theList.name )
    todoItems = theList.items
    adapter = new TodoAdapter( todoItems )
    listItemsView.setAdapter( adapter )

    // Event handlers...

    listItemsView.onItemClick { (view, posn, id) => toggleDone( posn ) }
    listItemsView.onItemLongClick { (view, posn, id) => editDialog.doEdit(posn)}
    findView( TR.addButton ).onClick { doAdd }
    newItemText.onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }
  }

  def doAdd = {
    val str = newItemText.getText.toString
    if (! str.equals( "" ) ) {
      todoItems += TodoItem( str, false )
      adapter.notifyDataSetChanged()
      newItemText.setText("")
    }
  }

  def setItemDescription( posn: Int, desc: String ) = {
    todoItems( posn ).description = desc
    adapter.notifyDataSetChanged()
  }

  def toggleDone( posn: Int ) = {
    todoItems( posn ).isDone = !todoItems( posn ).isDone
    adapter.notifyDataSetChanged()
  }

  def removeItem( posn: Int ) = {
    todoItems.remove( posn )
    adapter.notifyDataSetChanged
  }
}

class TodosAdapter
extends IndexedSeqAdapter( Todo.lists, itemViewResourceId = R.layout.todos_row){
  override def fillView( view: View, position: Int ) = {
    view.asInstanceOf[ TextView ].setText( getItem( position ).name )
  }
}

class KillListDialog( base: TodosActivity ) 
 extends Dialog( base, layoutResourceId = R.layout.kill_todo_list ) 
 with ViewFinder {

   var victimPosn = -1

   findView( TR.deleteButton ).onClick{ base.removeList( victimPosn ); dismiss }
   findView( TR.cancelButton ).onClick{ dismiss }

   def confirm( posn: Int ) = {
     victimPosn = posn
     findView( TR.victimText ).setText( "Delete " + Todo.lists(posn).name + "?")
     show
   }
}

class TodosActivity 
 extends Activity( layoutResourceId = R.layout.all_todos ) with ViewFinder {

  lazy val adapter = new TodosAdapter
  lazy val listsView = findView( TR.listsView )
  lazy val killDialog = new KillListDialog( this )

  onCreate {

    listsView.setAdapter( adapter )
    listsView.onItemClick { (view, posn, id) => viewList( posn ) }
    listsView.onItemLongClick { (view, posn, id) => killDialog.confirm( posn )}

    findView( TR.addButton ).onClick { doAdd }
    findView( TR.newListName ).onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }
  }

  def doAdd = {
    val str = findView( TR.newListName ).getText.toString
    if (! str.equals( "" ) ) {
      Todo.lists += TodoList( str )
      adapter.notifyDataSetChanged
      findView( TR.newListName ).setText("")
    }
  }

  def viewList( posn: Int ) {
    val intent = new Intent( this, classOf[TodoActivity] )
    intent.putExtra( Todo.listNumKey, posn )
    startActivity( intent )
  }

  def removeList( posn: Int ) = {
    Todo.lists.remove( posn )
    adapter.notifyDataSetChanged
  }

}

