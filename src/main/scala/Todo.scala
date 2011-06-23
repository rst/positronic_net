package rst.todo

import org.triplesec.Button
import org.triplesec.IndexedSeqAdapter
import org.triplesec.Dialog
import org.triplesec.EditText

import _root_.android.os.Bundle

import _root_.android.app.Activity

import _root_.android.util.Log
import _root_.android.content.Context

import _root_.android.widget.AdapterView
import _root_.android.widget.TextView
import _root_.android.widget.ListView

import _root_.android.view.View.OnKeyListener
import _root_.android.view.View.OnClickListener
import _root_.android.view.KeyEvent
import _root_.android.view.View
import _root_.android.view.ViewGroup
import _root_.android.view.LayoutInflater

import scala.collection.mutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer

case class TodoItem( var description: String, var isDone: Boolean )

class TodoAdapter(seq: IndexedSeq[TodoItem]) 
extends IndexedSeqAdapter( seq, itemViewResourceId = R.layout.todo_row ) {

  override def fillView( view: View, position: Int ) = {
    view.asInstanceOf[ TextView ].setText(getItem( position ).description)
  }
}

class EditDialog( base: TodoActivity, 
                  todos: ArrayBuffer[TodoItem] ) 
extends Dialog( base, layoutResourceId = R.layout.dialog ) {

  val editTxt = findView( TR.dialogEditText )
  var editingPosn: Int = -1

  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave }

  findView( TR.saveButton ).onClick { doSave }
  findView( TR.deleteButton ).onClick { doDelete }
  
  def doSave = {
    todos( editingPosn ).description = editTxt.getText().toString()
    base.adapter.notifyDataSetChanged()
    dismiss()
  }

  def doDelete = {
    todos.remove( editingPosn )
    base.adapter.notifyDataSetChanged()
    dismiss()
  }
    
  def doEdit( posn: Int ) = {
    editingPosn = posn
    editTxt.setText( todos(posn).description )
    show()
  }
  
}

class TodoActivity extends Activity {

  lazy val todoItems = new ArrayBuffer[TodoItem]
  lazy val adapter = new TodoAdapter(todoItems)
  lazy val editDialog = new EditDialog( this, todoItems )
  lazy val myListView = findViewById(R.id.myListView).asInstanceOf[ListView]
  lazy val myEditText = findViewById(R.id.myEditText).asInstanceOf[EditText]

  override def onCreate(savedInstanceState: Bundle) {

    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    myListView.setAdapter(adapter)
    myListView.setOnItemClickListener(new AdapterView.OnItemClickListener{
      def onItemClick(a: AdapterView[_], v:View, posn: Int, id: Long) = {
        editDialog.doEdit( posn )
      }
    })
    
    findViewById(R.id.addButton).asInstanceOf[Button].onClick { doAdd }
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
}
