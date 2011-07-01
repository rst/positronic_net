package org.positronic.ui

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

import org.positronic.util.AppFacility
import org.positronic.util.ChangeNotifications

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
    recreateInstanceState( b )
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

  def saveInstanceState( b: Bundle ) = {}
  def recreateInstanceState( b: Bundle ) = {}
  def restoreInstanceState( b: Bundle ) = {}

  override def onSaveInstanceState( b: Bundle ) = {
    super.onSaveInstanceState( b )
    saveInstanceState( b )
  }

  override def onRestoreInstanceState( b: Bundle ) = {
    super.onRestoreInstanceState( b )
    restoreInstanceState( b )
  }

  // Alternate overloadings of some standard methods, for convenience.

  def runOnUiThread( thunk: => Unit ):Unit = {
    this.runOnUiThread( new Runnable {
      def run() = { thunk }
    })
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

class Button( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Button( context, attrs ) 
 with PositronicHandlers

class EditText( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.EditText( context, attrs ) 
 with PositronicHandlers

class TextView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.TextView( context, attrs ) 
 with PositronicHandlers

class ListView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.ListView( context, attrs ) 
 with PositronicHandlers 
 with PositronicItemHandlers

class Dialog( context: Context, theme: Int = 0, layoutResourceId: Int = 0 )
 extends android.app.Dialog( context, theme ) with PositronicViewOps {

  if ( layoutResourceId != 0 )
    setContentView( layoutResourceId )
}

class Activity( layoutResourceId: Int = 0,
                optionsMenuResourceId: Int = 0,
                contextMenuResourceId: Int = 0
              )
 extends android.app.Activity
 with PositronicActivityHelpers 
{
  override def onCreate( b: Bundle ) = {
    super.onCreate( b, layoutResourceId )
  }

  override def onCreateOptionsMenu( menu: Menu ):Boolean = {
    if (optionsMenuResourceId == 0) {
      return false
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
      return false
    }
    this.rememberViewForContextMenu( view )
    getMenuInflater.inflate( contextMenuResourceId, menu )
    return true
  }
}

// Adapters for Scala collections.  Also support an alternative
// API which DRYs up common invocation patterns.
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
extends _root_.android.widget.BaseAdapter {

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
        createView( parent )
      }

    fillView( view, position )
    return view
  }

  def createView(parent: ViewGroup): View = {
    assert( itemViewResourceId != 0 )
    inflater.inflate( itemViewResourceId, parent, false )
  }

  def fillView( view: View, position: Int ) = {
    val textView = 
      (if (itemTextResourceId != 0)
        view.findViewById( itemTextResourceId )
       else
         view).asInstanceOf[ android.widget.TextView ]

    textView.setText( getItem( position ).toString )
  }

  // Accessors.  

  def getItem(position: Int):T = seq(position)
  def getItemId(position: Int) = getItem(position).hashCode()
  def getCount = seq.size
}
