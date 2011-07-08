package org.positronicnet.sample.todo

import org.positronicnet.ui.IndexedSeqAdapter
import org.positronicnet.ui.PositronicDialog
import org.positronicnet.ui.PositronicActivity
import org.positronicnet.ui.CursorSourceAdapter

import android.app.Activity
import android.os.Bundle
import android.content.Context
import android.content.Intent
import android.util.AttributeSet
import android.util.Log
import android.view.KeyEvent
import android.view.View
import android.view.ContextMenu
import android.widget.TextView
import android.widget.Toast
import android.graphics.Paint
import android.graphics.Canvas

// Adapter to wire up TodoList changes to the UI.
//
// Registers with the "source" (our TodoLists singleton) to be
// notified whenever its underlying data set is changed (or reloaded),
// so long as the activity is running.

class TodosAdapter( activity: PositronicActivity )
 extends CursorSourceAdapter( activity, 
                              source = TodoLists.lists,
                              converter = TodoList.fromCursor(_),
                              itemViewResourceId = R.layout.todos_row )
{
  def bindItem( view: View, list: TodoList ) =
    view.asInstanceOf[ TextView ].setText( list.name )
}

// Activity that uses it:

class TodosActivity 
 extends PositronicActivity( layoutResourceId = R.layout.all_todos ) 
 with ViewFinder 
{
  lazy val listsView = findView( TR.listsView )
  lazy val renameDialog = new EditStringDialog( this )

  onCreate {

    useOptionsMenuResource( R.menu.lists_view_menu )
    useContextMenuResource( R.menu.lists_context_menu )

    // Wire listsView to the database

    useAppFacility( TodoDb )            // Open DB; arrange to close on destroy
    listsView.setAdapter( new TodosAdapter( this ))

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

  // Determining relevant context for the ContextMenu

  def getContextItem( menuInfo: ContextMenu.ContextMenuInfo, view: View ) =
    listsView.selectedContextMenuItem( menuInfo ).asInstanceOf[ TodoList ]

  // Running UI commands

  def doAdd = {
    val str = findView( TR.newListName ).getText.toString
    if ( str != "" ) {
      TodoLists.addList( name = str )
      findView( TR.newListName ).setText("")
    }
  }

  def doRename( list: TodoList ) =
    renameDialog.doEdit( list.name ){ 
      TodoLists.setListName( list, _ )
    }

  def doDelete( list: TodoList ) = {
    TodoLists.removeList( list )
    toast( R.string.list_deleted, Toast.LENGTH_LONG )
  }

  def doUndelete = { 
    if (TodoLists.numDeletedLists.value > 0) TodoLists.undelete
    else toast( R.string.undeletes_exhausted )
  }

  def viewListAt( posn: Int ) {
    val intent = new Intent( this, classOf[TodoActivity] )
    val theList = listsView.getAdapter.getItem( posn ).asInstanceOf[ TodoList ]
    TodoList.intoIntent( theList, intent )
    startActivity( intent )
  }
}

// The activity's generic support dialog (used by the other activity as well).

class EditStringDialog( base: PositronicActivity )
 extends PositronicDialog( base, layoutResourceId = R.layout.dialog ) 
 with ViewFinder 
{
  val editTxt = findView( TR.dialogEditText )
  var saveHandler: ( String => Unit ) = null

  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }

  findView( TR.cancelButton ).onClick { dismiss }
  findView( TR.saveButton ).onClick { doSave; dismiss }
  
  def doSave = saveHandler( editTxt.getText.toString )

  def doEdit( str: String )( handler: String => Unit ) = { 
    editTxt.setText( str )
    saveHandler = handler
    show
  }
}

// And now, the other activity, which manages an individual todo list's items.

class TodoActivity 
 extends PositronicActivity( layoutResourceId = R.layout.todo_one_list ) 
 with ViewFinder 
{
  var theList: TodoList = null
  var showingDoneItems = true

  lazy val newItemText = findView( TR.newItemText )
  lazy val listItemsView = findView( TR.listItemsView )
  lazy val editDialog = new EditStringDialog( this )

  onCreate{

    useOptionsMenuResource( R.menu.items_view_menu )
    useContextMenuResource( R.menu.item_context_menu )

    // Setup --- get list out of our Intent, and hook up the listItemsView

    theList = TodoList.fromIntent( getIntent )
    setTitle( "Todo for: " + theList.name )

    useAppFacility( TodoDb )
    listItemsView.setAdapter( new TodoItemsAdapter( this, theList ) )

    // Event handlers...

    listItemsView.onItemClick {( view, posn, id ) =>
      toggleDone( getDisplayedItem( posn )) }

    findView( TR.addButton ).onClick { doAdd }
    newItemText.onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }

    onOptionsItemSelected( R.id.delete_where_done ) { deleteWhereDone }
    onOptionsItemSelected( R.id.undelete ) { undelete }
    onOptionsItemSelected( R.id.hide_done ) { setShowingDoneItems( false ) }
    onOptionsItemSelected( R.id.show_done ) { setShowingDoneItems( true ) }

    registerForContextMenu( listItemsView )

    onContextItemSelected( R.id.edit ){ 
      (menuInfo, view) => doEdit( getContextItem( menuInfo, view ))
    }
    onContextItemSelected( R.id.toggledone ){ 
      (menuInfo, view) => toggleDone( getContextItem( menuInfo, view ))
    }
  }

  // UI instance state

  override def onSaveInstanceState( b: Bundle ) = 
    b.putBoolean( "showing_done_items", showingDoneItems )

  override def onRestoreInstanceState( b: Bundle ) = 
    setShowingDoneItems( b.getBoolean( "showing_done_items" ) )

  // Dealing with mode switching

  def setShowingDoneItems( newValue: Boolean ) = {
    showingDoneItems = newValue
    // Query swapping will go here.
  }

  onPrepareOptionsMenu { menu =>
    // Hide mode-switch item which switches to the current mode...
    menu.findItem( R.id.hide_done ).setVisible(  showingDoneItems )
    menu.findItem( R.id.show_done ).setVisible( !showingDoneItems )
  }

  // Finding target items for listItemsView taps (including the ContextMenu)

  def getContextItem( menuInfo: ContextMenu.ContextMenuInfo, view: View ) =
    listItemsView.selectedContextMenuItem( menuInfo ).asInstanceOf[ TodoItem ]

  def getDisplayedItem( posn: Int ) = 
    listItemsView.getAdapter.getItem( posn ).asInstanceOf[ TodoItem ]

  // Running UI commands

  def doAdd = {
    val str = newItemText.getText.toString
    if ( str != "" ) {
      theList.addItem( description = str, isDone = false )
      newItemText.setText("")
    }
  }

  def doEdit( it: TodoItem ) = 
    editDialog.doEdit( it.description ) {
       theList.setItemDescription( it, _ )
    }

  def toggleDone( it: TodoItem ) = theList.setItemDone( it, !it.isDone )

  def deleteWhereDone = {
    if (theList.numDoneItems.value > 0) 
      theList.deleteWhereDone
    else
      toast( R.string.no_tasks_done )
  }

  def undelete = {
    if (theList.numDeletedItems.value > 0)
      theList.undeleteItems
    else
      toast( R.string.undeletes_exhausted )
  }
}

class TodoItemsAdapter( activity: PositronicActivity, list: TodoList )
 extends CursorSourceAdapter( activity,
                              source = list.items,
                              converter = TodoItem.fromCursor(_),
                              itemViewResourceId = R.layout.todo_row )
{
  def bindItem( view: View, it: TodoItem ) =
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

