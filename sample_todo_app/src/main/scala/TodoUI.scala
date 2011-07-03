package rst.todo

import org.positronicnet.ui.IndexedSeqAdapter
import org.positronicnet.ui.PositronicDialog
import org.positronicnet.ui.PositronicActivity

import android.app.Activity
import android.os.Bundle
import android.content.Context
import android.content.Intent
import android.util.AttributeSet
import android.util.Log
import android.view.KeyEvent
import android.view.View
import android.view.ContextMenu
import android.widget.Toast
import android.widget.TextView
import android.graphics.Paint
import android.graphics.Canvas

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

// List adapter for the singleton "TodoLists" object, which is all known lists.

class TodosAdapter
extends IndexedSeqAdapter( TodoLists.lists, 
                           itemViewResourceId = R.layout.todos_row)
{
  override def bindView( view: View, list: TodoList ) =
    view.asInstanceOf[ TextView ].setText( list.name )
}

// Activity that uses it.

class TodosActivity 
 extends PositronicActivity( layoutResourceId = R.layout.all_todos,
                             optionsMenuResourceId = R.menu.lists_view_menu,
                             contextMenuResourceId = R.menu.lists_context_menu) 
 with ViewFinder 
{
  lazy val listsView = findView( TR.listsView )

  onCreate {

    // Set things up

    val adapter = new TodosAdapter
    listsView.setAdapter( adapter )

    useAppFacility( TodoDb )            // Open DB; arrange to close on destroy

    onChangeTo( TodoLists ){ 
      // If we loaded a fresh copy on the DB thread, tell the adapter...
      lists => this.runOnUiThread{ adapter.resetSeq( lists )}
    }
    TodoLists.refreshFromDb

    // Listen for events on widgets

    listsView.onItemClick { (view, posn, id) => viewListAt( posn ) }

    findView( TR.addButton ).onClick { doAdd }
    findView( TR.newListName ).onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }

    onOptionsItemSelected( R.id.undelete ) { doUndelete }

    registerForContextMenu( listsView )

    onContextItemSelected( R.id.rename ){ 
      (menuInfo, view) => doRename( getContextItem( menuInfo, view ))
    }
    onContextItemSelected( R.id.delete ){ 
      (menuInfo, view) => doDelete( getContextItem( menuInfo, view ))
    }
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

  // Determining relevant context for the ContextMenu

  def getContextItem( menuInfo: ContextMenu.ContextMenuInfo, view: View ) =
    listsView.selectedContextMenuItem( menuInfo ).asInstanceOf[ TodoList ]

  // Running UI commands

  def doAdd = {
    val str = findView( TR.newListName ).getText.toString
    if ( str != "" ) {
      TodoLists.addList( name = str )
      findView( TR.newListName ).setText("")
      listsView.setSelection( TodoLists.lists.size - 1 )
    }
  }

  def doRename( list: TodoList ) = {
    new EditStringDialog( this, list.name )
      .onSave{ name => TodoLists.setListName( list, name ) }
      .show
  }

  def doDelete( list: TodoList ) = {
    TodoLists.removeList( list )
    toast( R.string.list_deleted, Toast.LENGTH_LONG )
  }

  def doUndelete = { 
    if (TodoLists.hasDeleted) TodoLists.undelete
    else toast( R.string.undeletes_exhausted )
  }

  def viewListAt( posn: Int ) {
    val intent = new Intent( this, classOf[TodoActivity] )
    intent.putExtra( TodoUI.listNumKey, posn )
    startActivity( intent )
  }
}

// And now, the other activity, which manages an individual todo list's items.

class TodoActivity 
 extends PositronicActivity( layoutResourceId = R.layout.todo_one_list,
                             optionsMenuResourceId = R.menu.items_view_menu,
                             contextMenuResourceId = R.menu.item_context_menu ) 
 with ViewFinder 
{
  var theList: TodoList = null

  lazy val newItemText = findView( TR.newItemText )
  lazy val listItemsView = findView( TR.listItemsView )

  onCreate{

    // Setup

    theList = TodoLists.lists( getIntent.getIntExtra( TodoUI.listNumKey, -1 ))
    setTitle( "Todo for: " + theList.name )

    val adapter = new TodoItemsAdapter( theList.items )
    listItemsView.setAdapter( adapter )

    useAppFacility( TodoDb )

    onChangeTo( theList ){ items => this.runOnUiThread{adapter.resetSeq(items)}}
    theList.refreshFromDb

    // Event handlers...

    listItemsView.onItemClick {( view, posn, id ) =>
      toggleDone( theList.items( posn )) }

    findView( TR.addButton ).onClick { doAdd }
    newItemText.onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }

    onOptionsItemSelected( R.id.delete_where_done ) { deleteWhereDone }
    onOptionsItemSelected( R.id.undelete ) { undelete }

    registerForContextMenu( listItemsView )

    onContextItemSelected( R.id.edit ){ 
      (menuInfo, view) => doEdit( getContextItem( menuInfo, view ))
    }
    onContextItemSelected( R.id.toggledone ){ 
      (menuInfo, view) => toggleDone( getContextItem( menuInfo, view ))
    }
  }

  // Determining relevant context for the ContextMenu

  def getContextItem( menuInfo: ContextMenu.ContextMenuInfo, view: View ) =
    listItemsView.selectedContextMenuItem( menuInfo ).asInstanceOf[ TodoItem ]

  // Running UI commands

  def doAdd = {
    val str = newItemText.getText.toString
    if ( str != "" ) {
      theList.addItem( description = str, isDone = false )
      newItemText.setText("")
      listItemsView.setSelection( theList.items.size - 1 )
    }
  }

  def doEdit( it: TodoItem ) = {
    new EditStringDialog( this, it.description )
      .onSave{ desc => theList.setItemDescription( it, desc ) }
      .show
  }

  def toggleDone( it: TodoItem ) = theList.setItemDone( it, !it.isDone )

  def deleteWhereDone = {
    if (theList.hasDoneItems) 
      theList.deleteWhereDone
    else
      toast( R.string.no_tasks_done )
  }

  def undelete = {
    if (theList.hasDeletedItems)
      theList.undelete
    else
      toast( R.string.undeletes_exhausted )
  }
}

class EditStringDialog( base: PositronicActivity, str: String )
 extends PositronicDialog( base, layoutResourceId = R.layout.dialog ) 
 with ViewFinder 
{
  val editTxt = findView( TR.dialogEditText )
  var saveHandler: ( String => Unit ) = null

  editTxt.setText( str )
  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }

  findView( TR.cancelButton ).onClick { dismiss }
  findView( TR.saveButton ).onClick { doSave; dismiss }
  
  def doSave = saveHandler( editTxt.getText.toString )

  def onSave( handler: String => Unit ):EditStringDialog = { 
    saveHandler = handler; 
    return this 
  }
}

class TodoItemsAdapter(seq: IndexedSeq[TodoItem]) 
 extends IndexedSeqAdapter( seq, itemViewResourceId = R.layout.todo_row ) 
{
  override def bindView( view: View, it: TodoItem ) =
    view.asInstanceOf[ TodoItemView ].setTodoItem( it )
}

// View for TodoItems:  TextView which adds strikethrough if the item "isDone" 

class TodoItemView( context: Context, attrs: AttributeSet = null )
 extends TextView( context, attrs ) 
{
   var theItem: TodoItem = null
   def getTodoItem = theItem

   def setTodoItem( item: TodoItem ) = {
     theItem = item
     setText( item.description )
     setPaintFlags( 
       if (item.isDone) getPaintFlags | Paint.STRIKE_THRU_TEXT_FLAG 
       else getPaintFlags & ~Paint.STRIKE_THRU_TEXT_FLAG
     )
   }
}


