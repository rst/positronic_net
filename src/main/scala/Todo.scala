package rst.todo

import org.triplesec.Button
import org.triplesec.IndexedSeqAdapter
import org.triplesec.Dialog

import _root_.android.os.Bundle

import _root_.android.app.Activity

import _root_.android.util.Log
import _root_.android.content.Context

import _root_.android.widget.AdapterView
import _root_.android.widget.EditText
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
    view.asInstanceOf[ TextView ].setText(getItemTyped( position ).description)
  }
}

class EditDialog( base: TodoActivity, 
                  todos: ArrayBuffer[TodoItem] ) 
extends Dialog( base, layoutResourceId = R.layout.dialog ) {

  val editTxt = findView( TR.dialogEditText )
  var editingPosn: Int = -1

  findView( TR.saveButton ).onClick {
    todos( editingPosn ).description = editTxt.getText().toString()
    base.adapter.notifyDataSetChanged()
    dismiss()
  }

  findView( TR.deleteButton ).onClick {
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

  override def onCreate(savedInstanceState: Bundle) {

    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    val myListView = findViewById(R.id.myListView).asInstanceOf[ListView]
    val myEditText = findViewById(R.id.myEditText).asInstanceOf[EditText]

    myListView.setAdapter(adapter)
    myListView.setOnItemClickListener(new AdapterView.OnItemClickListener{
      def onItemClick(a: AdapterView[_], v:View, posn: Int, id: Long) = {
        editDialog.doEdit( posn )
      }
    })
    
    findViewById(R.id.addButton).asInstanceOf[Button].onClick {
      todoItems += TodoItem( myEditText.getText.toString, false )
      adapter.notifyDataSetChanged()
      myEditText.setText("")
    }
  }
}
