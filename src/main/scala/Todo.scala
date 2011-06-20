package rst.todo

import _root_.android.os.Bundle
import _root_.android.app.Activity
import _root_.android.content.Context

import _root_.android.widget.BaseAdapter
import _root_.android.widget.EditText
import _root_.android.widget.TextView
import _root_.android.widget.ListView

import _root_.android.view.View.OnKeyListener
import _root_.android.view.KeyEvent
import _root_.android.view.View
import _root_.android.view.ViewGroup
import _root_.android.view.LayoutInflater

import scala.collection.mutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer

class TodoAdapter(seq: IndexedSeq[String]) extends BaseAdapter {

  def getView(position: Int, convertView: View, parent: ViewGroup):View = {
    val view = 
      if (convertView != null) convertView
      else parent.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
           .asInstanceOf[LayoutInflater].inflate(R.layout.todo_row,
                                                 parent, false)

    val textView = view.asInstanceOf[TextView]
    textView.setText(getItem(position))

    return textView
  }

  def getItemId(position: Int) = getItem(position).hashCode()
  def getItem(position: Int):String = seq(position)
  def getCount = seq.size
}

class TodoActivity extends Activity {
  override def onCreate(savedInstanceState: Bundle) {

    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    val myListView = findViewById(R.id.myListView).asInstanceOf[ListView]
    val myEditText = findViewById(R.id.myEditText).asInstanceOf[EditText]

    val todoItems = new ArrayBuffer[String]
    val adapter = new TodoAdapter(todoItems)
    myListView.setAdapter(adapter)
    myEditText.setOnKeyListener(new OnKeyListener {
      def onKey(v: View, keyCode: Int, event: KeyEvent):Boolean = {
        if (event.getAction() == KeyEvent.ACTION_DOWN &&
            keyCode == KeyEvent.KEYCODE_DPAD_CENTER) {
          todoItems += myEditText.getText.toString
          adapter.notifyDataSetChanged()
          myEditText.setText("")
          return true
        }
        return false
      }
    });
  }
}
