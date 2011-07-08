package org.positronicnet.ui

import _root_.android.content.Context
import _root_.android.util.AttributeSet
import _root_.android.view.LayoutInflater
import _root_.android.view.View
import _root_.android.view.ViewGroup
import _root_.android.view.Menu
import _root_.android.view.ContextMenu
import _root_.android.view.MenuItem
import _root_.android.os.Bundle
import _root_.android.widget.AdapterView
import _root_.android.widget.Toast
import _root_.android.util.Log

import org.positronicnet.util.AppFacility
import org.positronicnet.util.ChangeNotifications

import org.positronicnet.db.PositronicCursor // for CursorSourceAdapter
import _root_.android.database.Cursor

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait PositronicViewOps {
  // This would be the place to put findView, if we knew where
  // to find TypedResource.
}

// "JQuery-style" event listener declarations.  Fortunately, these
// don't conflict with the native API because they're alternate
// overloadings.

trait PositronicHandlers extends PositronicViewOps {

  def setOnClickListener( dummy: View.OnClickListener ): Unit

  def onClick(func: => Unit) = {
    setOnClickListener( new View.OnClickListener {
      def onClick( dummy: View ) = { func }
    })
  }

  def setOnKeyListener( dummy: View.OnKeyListener ): Unit

  def onKey(func: ( Int, android.view.KeyEvent ) => Boolean) = {
    setOnKeyListener( new View.OnKeyListener {
      def onKey( dummy: View, 
                 keyCode: Int, event: android.view.KeyEvent ):Boolean = {
        return func( keyCode, event )
      }
    })
  }

  // Handler for a *specific* key.  Arguable bug:  can only declare one!
  // Not hard to fix, but not needed for now.  Mark XXX TODO.

  def onKey(keyCode: Int, metaState: Int = 0)( func: => Unit ) = {
    setOnKeyListener( new View.OnKeyListener {
      def onKey( dummy: View, 
                 eventKeyCode: Int, event: android.view.KeyEvent ):Boolean = {
        if (eventKeyCode == keyCode && event.getMetaState == metaState) {
          func; return true
        } else {
          return false
        }
      }
    })
  }

}

// DRYer handlers for AdapterView-specific events.
//
// There is weirdness here; an onItemLongClick handler can return false
// to mark the event unhandled, but the onItemClick handler is defined
// to return Unit (a/k/a a Java void), and is just assumed to have handled
// the event.  What?!
//
// Our onItemLongClick is like onItemClick; it takes a handler which returns
// Unit, and always returns 'true' (event handled) to the framework.  If you
// really want the choice, there's onItemLongClick maybe, which takes a 
// handler returning a Boolean.  (We can't use overloading here, because
// the handler types are the same after erasure.)

trait PositronicItemHandlers {

  def setOnItemClickListener( l: AdapterView.OnItemClickListener ): Unit
  def setOnItemLongClickListener( l: AdapterView.OnItemLongClickListener ): Unit

  def onItemClick( func: (View, Int, Long) => Unit) = {
    setOnItemClickListener( new AdapterView.OnItemClickListener {
      def onItemClick( parent: AdapterView[_], view: View,
                       position: Int, id: Long ) = { 
        func( view, position, id ) 
      }
    })
  }
   
  def onItemLongClick( func: (View, Int, Long) => Unit) = {
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id ); return true
      }
    })
  }
   
  def onItemLongClickMaybe( func:(View, Int, Long) => Boolean)={
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id )
      }
    })
  }

  def getItemAtPosition( posn: Int ):Object

  def selectedContextMenuItem( info: ContextMenu.ContextMenuInfo ):Object = {
    val posn = info.asInstanceOf[ AdapterView.AdapterContextMenuInfo ].position
    return getItemAtPosition( posn )
  }
   
}

trait PositronicActivityHelpers
 extends _root_.android.app.Activity
{
  // Correctly scope change handling, so we don't wind up processing
  // change notifications for defunct activities.  Meant to be called
  // from onCreate; automatically unregisters the handler onDestroy.

  def onChangeTo[T]( frob: ChangeNotifications[T] )( handler: T => Unit ) {
    frob.onChange( this ){ handler }
    this.onDestroy{ frob.stopChangeNotifications( this ) }
  }

  // Likewise for AppFacilities...

  def useAppFacility( f: AppFacility ) {
    f.openInContext( this.getApplicationContext )
    this.onDestroy{ f.close }
  }

  // Handlers for lifecycle events.  The idea here is simply to
  // eliminate the ceremony of having to call super.foo() when
  // redefining each of these.
  //
  // Note also the variant handling of instance state --- the
  // explicit saveInstanceState( Bundle ) and recreateInstanceState( Bundle )
  // methods, called from the "on..." variant, or onCreate, respectively,
  // again to eliminate ceremony.

  class Handlers extends ArrayBuffer[ () => Unit ] {
    def runAll = for (handler <- this) { handler() }
  }

  var onCreateNotifier  = new Handlers
  var onRestartNotifier = new Handlers
  var onStartNotifier   = new Handlers
  var onResumeNotifier  = new Handlers
  var onPauseNotifier   = new Handlers
  var onStopNotifier    = new Handlers
  var onDestroyNotifier = new Handlers

  override def onCreate( b: Bundle ) {
    onCreate( b, 0 )
  }

  def onCreate( b: Bundle, layoutResourceId: Int ) = {
    super.onCreate( b )
    if (layoutResourceId != 0) { setContentView( layoutResourceId ) }
    if ( b != null ) recreateInstanceState( b )
    onCreateNotifier.runAll
  }

  def onCreate( thunk: => Unit ) = { onCreateNotifier.append( () => thunk ) }

  override def onRestart = { 
    super.onRestart(); 
    onRestartNotifier.runAll
  }
  
  def onRestart( thunk: => Unit ) = { onRestartNotifier.append( () => thunk )}

  override def onResume = { 
    super.onResume(); 
    onResumeNotifier.runAll
  }
  
  def onResume( thunk: => Unit ) = { onResumeNotifier.append( () => thunk ) }

  override def onPause = { 
    super.onPause(); 
    onPauseNotifier.runAll
  }
  
  def onPause( thunk: => Unit ) = { onPauseNotifier.append( () => thunk ) }

  override def onStop = { 
    super.onStop(); 
    onStopNotifier.runAll
  }
  
  def onStop( thunk: => Unit ) = { onStopNotifier.append( () => thunk ) }

  override def onDestroy = { 
    super.onDestroy(); 
    onDestroyNotifier.runAll
  }
  
  def onDestroy( thunk: => Unit ) = { onDestroyNotifier.append( () => thunk )}

  // Versions of onSaveInstanceState and friends which eliminate
  // the super.foo() noise, and only get called if there *is* a
  // bundle to unpack.

  def saveInstanceState( b: Bundle ) = {}
  def recreateInstanceState( b: Bundle ) = {}
  def restoreInstanceState( b: Bundle ) = {}

  override def onSaveInstanceState( b: Bundle ) = {
    super.onSaveInstanceState( b )
    if ( b != null ) saveInstanceState( b )
  }

  override def onRestoreInstanceState( b: Bundle ) = {
    super.onRestoreInstanceState( b )
    if ( b != null ) restoreInstanceState( b )
  }

  // Alternate overloadings of some standard methods, for convenience.

  def runOnUiThread( thunk: => Unit ):Unit = {
    this.runOnUiThread( new Runnable {
      def run() = { thunk }
    })
  }

  // Shorthands for dealing with menus

  private var optionsMenuResourceId = 0
  private var contextMenuResourceId = 0

  def useOptionsMenuResource( id: Int ) = optionsMenuResourceId = id
  def useContextMenuResource( id: Int ) = contextMenuResourceId = id

  override def onCreateOptionsMenu( menu: Menu ):Boolean = {
    if (optionsMenuResourceId == 0) {
      // We might have gotten mixed into an activity that has its own ideas...
      return super.onCreateOptionsMenu( menu )
    }
    getMenuInflater.inflate( optionsMenuResourceId, menu )
    return true
  }

  override def onCreateContextMenu( menu: ContextMenu, 
                                    view: View, 
                                    info: ContextMenu.ContextMenuInfo
                                  ):Unit = 
  {
    if (contextMenuResourceId == 0) {
      // We might have gotten mixed into an activity with its own ideas... 
      return super.onCreateContextMenu( menu, view, info )
    }
    this.rememberViewForContextMenu( view )
    getMenuInflater.inflate( contextMenuResourceId, menu )
    return true
  }

  val optionsItemMap = new HashMap[ Int, (() => Unit) ]

  def onOptionsItemSelected( id: Int )( thunk: => Unit ) = {
    optionsItemMap( id ) = (() => thunk)
  }

  override def onOptionsItemSelected( it: MenuItem ):Boolean = {
    super.onOptionsItemSelected( it )
    val handler = optionsItemMap( it.getItemId )
    if (handler == null)
      return false
    else {
      handler()
      return true
    }
  }

  type ContextItemHandler = (( ContextMenu.ContextMenuInfo, View ) => Unit )

  val contextItemMap = new HashMap[ Int, ContextItemHandler ]
  var contextMenuView: View = null

  def rememberViewForContextMenu( v: View ) = { contextMenuView = v }

  def onContextItemSelected( id: Int )( handler: ContextItemHandler ) = {
    contextItemMap( id ) = handler
  }

  override def onContextItemSelected( it: MenuItem ):Boolean = {
    super.onOptionsItemSelected( it )
    val handler = contextItemMap( it.getItemId )
    if (handler == null)
      return false
    else {
      handler( it.getMenuInfo, contextMenuView )
      return true
    }
  }

  var onPrepareOptionsMenuHandlers = new ArrayBuffer[ Menu => Unit ]

  override def onPrepareOptionsMenu( m: Menu ):Boolean = { 
    super.onPrepareOptionsMenu( m ); 
    for( handler <- onPrepareOptionsMenuHandlers ) { handler( m ) }
    return true;
  }
  
  def onPrepareOptionsMenu( handler: Menu => Unit ) = 
    onPrepareOptionsMenuHandlers.append( handler )

  // And these, just to cut down on noise.

  def toast( msgResId: Int, duration: Int = Toast.LENGTH_SHORT ):Unit = {
    Toast.makeText( this, msgResId, duration ).show
  }
} 

// Widgets (and other things) with our traits premixed in.
//
// Note that we don't (yet) provide allthe constructor variants
// with "theme" arguments, since we've got a bit of a catch-22 
// with them.
//
// Scala classes must have a single "main" constructor which all the
// others call --- they can't directly invoke overloaded constructors
// in the base class.  If we supported all of the "style"-as-arg
// variants (e.g., the three-argument constructors for "Button", etc.,
// with "style" as the third ag), then the other constructors would
// have to supply the default value.  But the proper value to supply,
// at least in the Button case, from consulting the android source
// directly, is something fished out of com.android.internal.R which
// I'm not sure how to access from code.
//
// The only obvious workaround would be to have Foo and StyledFoo
// variants, with the StyledFoo having the three-arg constructor.
// But since the guts of everything is in traits, that's not hard.

class PositronicButton( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Button( context, attrs ) 
 with PositronicHandlers

class PositronicEditText( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.EditText( context, attrs ) 
 with PositronicHandlers

class PositronicTextView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.TextView( context, attrs ) 
 with PositronicHandlers

class PositronicListView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.ListView( context, attrs ) 
 with PositronicHandlers 
 with PositronicItemHandlers

class PositronicDialog( context: Context, 
                        theme: Int = 0, 
                        layoutResourceId: Int = 0 )
 extends android.app.Dialog( context, theme ) 
 with PositronicViewOps 
{
  if ( layoutResourceId != 0 )
    setContentView( layoutResourceId )
}

class PositronicActivity( layoutResourceId: Int = 0 )
 extends android.app.Activity
 with PositronicActivityHelpers 
{
  override def onCreate( b: Bundle ) = super.onCreate( b, layoutResourceId )
}

// Adapter for cursors produced by PositronicDb queries.
// Automatically handles a fair amount of the usual typecasting
// gubbish...

abstract class CursorSourceAdapter[T <: AnyRef]( 
  activity: PositronicActivityHelpers,
  converter: PositronicCursor => T,
  source: ChangeNotifications[PositronicCursor] = null,
  itemViewResourceId: Int = 0
)
 extends _root_.android.widget.CursorAdapter( activity, null )
{
  var inflater: LayoutInflater = null

  if (source != null) {
    source.onChange( this ) {
      cursor => activity.runOnUiThread{ this.changeCursor( cursor ) }
    }
    activity.onDestroy{ source.stopChangeNotifications( this ) }
  }

  def newView( context: Context, 
               cursor: android.database.Cursor,
               parent: ViewGroup ): View =
  {
    if (itemViewResourceId == 0)
      throw new RuntimeException( "QueryAdapter with itemViewResourceId unset"+
                                  " and newView not overridden" )
    if (inflater == null) {
      inflater = 
        parent.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
          .asInstanceOf[LayoutInflater]
    }

    return inflater.inflate( itemViewResourceId, parent, false )
  }
                 
  override def bindView( view: View, context: Context, cursor: Cursor ) = {
    val item = converter( cursor.asInstanceOf[ PositronicCursor ] )
    bindItem( view, item )
  }

  def bindItem( view: View, item: T )

  override def getItem( posn: Int ): T = {
    val baseValue = super.getItem( posn )
    if (baseValue == null)
      return null.asInstanceOf[T]
    else
      return converter( baseValue.asInstanceOf[ PositronicCursor ])
  }
}

// Adapter for Scala IndexedSeq's.
//
// Supports newView and BindView methods, analogous to those
// provided by the base framework's CursorAdapter (though
// newView takes only the parent ViewGroup as an argument).
//
// Note that the "T <: Object" restriction is needed so that
// our "getItem( _: Int ):T" is compatible with the declared
// "getItem( _: Int ): java.lang.Object" in the Adapter interface.
// So, if you really want an adapter for an IndexedSeq[Long],
// you're on your own.

class IndexedSeqAdapter[T <: Object](var seq:IndexedSeq[T] = new ArrayBuffer[T],
                                     itemViewResourceId: Int = 0, 
                                     itemTextResourceId: Int = 0
                                    ) 
  extends _root_.android.widget.BaseAdapter 
{
  var inflater: LayoutInflater = null

  // Method to reset the sequence if a new copy was (or might have been)
  // loaded off the UI thread.

  def resetSeq( newSeq: IndexedSeq[T] ) = {
    seq = newSeq
    notifyDataSetChanged
  }

  def getView( position: Int, convertView: View, parent: ViewGroup ):View = {

    val view = 
      if (convertView != null) {
        convertView
      }
      else {
        if (inflater == null) {
          inflater = 
            parent.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
             .asInstanceOf[LayoutInflater]
        }
        newView( parent )
      }

    bindView( view, getItem( position ))
    return view
  }

  def newView( parent: ViewGroup ): View = {
    assert( itemViewResourceId != 0 )
    inflater.inflate( itemViewResourceId, parent, false )
  }

  def bindView( view: View, item: T ) = {
    val textView = 
      (if (itemTextResourceId != 0)
        view.findViewById( itemTextResourceId )
       else
         view).asInstanceOf[ android.widget.TextView ]

    textView.setText( item.toString )
  }

  // Accessors.  

  def getItem(position: Int):T = seq(position)
  def getItemId(position: Int) = getItem(position).hashCode()
  def getCount = seq.size
}
