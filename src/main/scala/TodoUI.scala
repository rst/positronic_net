package rst.todo

import org.positronic.ui.Button
import org.positronic.ui.IndexedSeqAdapter
import org.positronic.ui.Dialog
import org.positronic.ui.EditText
import org.positronic.ui.TextView
import org.positronic.ui.Activity
import org.positronic.ui.ListView

import _root_.android.os.Bundle
import _root_.android.content.Context
import _root_.android.content.Intent
import _root_.android.util.AttributeSet
import _root_.android.util.Log
import _root_.android.view.KeyEvent
import _root_.android.view.View
import _root_.android.graphics.Paint
import _root_.android.graphics.Canvas

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

// Main UI to deal with lists, as exported by the "Todo" manager
// object, starting with an activity to manage the set of available
// lists.

object TodoUI {
  val listNumKey = "listNum"            // For intents; see below.
}

class TodosActivity 
 extends Activity( layoutResourceId = R.layout.all_todos,
                   optionsMenuResourceId = R.menu.lists_view_menu ) 
 with ViewFinder {

  onCreate {

    val adapter = new TodosAdapter
    val listsView = findView( TR.listsView )
    listsView.setAdapter( adapter )

    useAppFacility( TodoDb )

    onChangeTo( Todo ){ lists => this.runOnUiThread{ adapter.resetSeq( lists )}}
    Todo.refreshFromDb

    listsView.onItemClick { (view, posn, id) => viewList( posn ) }
    listsView.onItemLongClick { (view, posn, id) => 
      new KillListDialog( this, posn ).show }

    findView( TR.addButton ).onClick { doAdd }
    findView( TR.newListName ).onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }

    onOptionsItemSelected( R.id.undelete ) { undelete }
  }

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
    intent.putExtra( TodoUI.listNumKey, posn )
    startActivity( intent )
  }

  def removeList( posn: Int ) = { Todo.removeList( posn ) }

  def undelete = { 
    if (Todo.hasDeleted) Todo.undelete
    else toast( R.string.undeletes_exhausted )
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
 extends Activity( layoutResourceId = R.layout.todo_one_list,
                   optionsMenuResourceId = R.menu.items_view_menu ) 
 with ViewFinder 
{
  var theList: TodoList = null

  lazy val editDialog = new EditItemDialog(this,theList) // constructed once
  lazy val newItemText = findView( TR.newItemText )

  onCreate{

    // Setup

    theList = Todo.lists( getIntent.getIntExtra( TodoUI.listNumKey, -1 ))
    setTitle( "Todo for: " + theList.name )

    val adapter = new TodoItemsAdapter( theList.items )
    val listItemsView = findView( TR.listItemsView )
    listItemsView.setAdapter( adapter )

    useAppFacility( TodoDb )

    onChangeTo( theList ){ items => this.runOnUiThread{adapter.resetSeq(items)}}
    theList.refreshFromDb

    // Event handlers...

    listItemsView.onItemClick { (view, posn, id) => toggleDone( posn ) }
    listItemsView.onItemLongClick { (view, posn, id) => editDialog.doEdit(posn)}
    findView( TR.addButton ).onClick { doAdd }
    newItemText.onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }

    onOptionsItemSelected( R.id.delete_where_done ) { deleteWhereDone }
    onOptionsItemSelected( R.id.undelete ) { undelete }
  }

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

  def deleteWhereDone {
    if (theList.hasDoneItems) 
      theList.deleteWhereDone
    else
      toast( R.string.no_tasks_done )
  }

  def undelete {
    if (theList.hasDeletedItems)
      theList.undelete
    else
      toast( R.string.undeletes_exhausted )
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
  findView( TR.cancelButton ).onClick { dismiss }
  
  def doSave = {
    base.setItemDescription( editingPosn, editTxt.getText.toString )
  }

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


