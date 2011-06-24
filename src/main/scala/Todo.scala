package rst.todo

import org.triplesec.Button
import org.triplesec.IndexedSeqAdapter
import org.triplesec.Dialog
import org.triplesec.EditText
import org.triplesec.TextView
import org.triplesec.Activity
import org.triplesec.ListView

import _root_.android.content.Context
import _root_.android.util.AttributeSet
import _root_.android.util.Log
import _root_.android.view.KeyEvent
import _root_.android.view.View
import _root_.android.graphics.Paint
import _root_.android.graphics.Canvas

import scala.collection.mutable.ArrayBuffer

case class TodoItem( var description: String, var isDone: Boolean )

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
extends Dialog( base, layoutResourceId = R.layout.dialog ) {

  val editTxt = findView( TR.dialogEditText )
  var editingPosn: Int = -1

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

class TodoActivity extends Activity( layoutResourceId = R.layout.main ) {

  lazy val todoItems = new ArrayBuffer[TodoItem]
  lazy val adapter = new TodoAdapter(todoItems)
  lazy val editDialog = new EditDialog( this, todoItems )
  lazy val myListView = findView( TR.myListView )
  lazy val myEditText = findView( TR.myEditText )

  onCreate{

    // Setup
    myListView.setAdapter( adapter )

    // Event handlers...
    myListView.onItemClick { (view, posn, id) => toggleDone( posn ) }
    myListView.onItemLongClick { (view, posn, id) => editDialog.doEdit( posn )}
    findView( TR.addButton ).onClick { doAdd }
    myEditText.onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }
  }

  def doAdd {
    val str = myEditText.getText.toString
    if (! str.equals( "" ) ) {
      todoItems += TodoItem( str, false )
      adapter.notifyDataSetChanged()
      myEditText.setText("")
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
