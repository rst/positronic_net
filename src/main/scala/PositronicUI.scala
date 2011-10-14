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
import _root_.android.view.KeyEvent
import _root_.android.view.View.OnKeyListener

import org.positronicnet.facility.AppFacility
import org.positronicnet.notifications.Notifier

import org.positronicnet.content.PositronicCursor // for CursorSourceAdapter
import _root_.android.database.Cursor

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/** Mixin trait for view subclasses which provides 
  * "JQuery-style" event listener declarations.  Fortunately, these
  * don't conflict with the native API because they're alternate
  * overloadings.
  *
  * NB the Scaladoc here is more readable if inherited methods
  * are omitted.
  */

trait PositronicHandlers {

  def setOnClickListener( dummy: View.OnClickListener ): Unit

  /** Call `func` when there is a click on this view. */

  def onClick(func: => Unit) = {
    setOnClickListener( new View.OnClickListener {
      def onClick( dummy: View ) = { func }
    })
  }

  def setOnKeyListener( dummy: OnKeyListener ): Unit

  private var genericOnKeyHandler: ((Int, KeyEvent) => Boolean) = null
  private var keyCodeHandlers: Map[ (Int,Int), (()=>Unit) ] = Map.empty
  private var haveInstalledKeyListener = false

  private [positronicnet]
  def installOnKeyListener = {
    if (!haveInstalledKeyListener) {
      val target = this
      setOnKeyListener( new OnKeyListener {
        def onKey( v: View, code: Int, ev: KeyEvent ) = 
          target.positronicKeyDispatch( code, ev.getMetaState, ev.getAction, ev)
      })
      haveInstalledKeyListener = true
    }
  }

  // Our dispatch logic, exposed here for the sake of tests.
  // (At time of writing, key dispatch stuff is awkward with Robolectric.)

  private [positronicnet]
  def positronicKeyDispatch( keyCode: Int, metaState: Int, 
                             action: Int, ev: KeyEvent ): Boolean = {
    if (action == KeyEvent.ACTION_DOWN) {
      for (handler <- keyCodeHandlers.get((keyCode, metaState))) {
        handler()
        return true
      }
    }
    if (genericOnKeyHandler != null) {
      return genericOnKeyHandler( keyCode, ev )
    }
    return false
  }

  /** Call the given `func` when this view has keyboard focus, and any
    * key is hit.  Arguments to `func` are the integer key code, and the
    * key event.  The `func` should return `true` if it handled the event,
    * `false` otherwise.
    *
    * Handlers declared for specific keycodes with `onKey( code ){ ... }`
    * take precedence, and will always override the generic handler here.
    */

  def onKey(func: ( Int, android.view.KeyEvent ) => Boolean):Unit = {
    installOnKeyListener
    genericOnKeyHandler = func
  }

  /** Call the given `func` on a keydown event (`KeyEvent.ACTION_DOWN`)
    * with the given keycode, and the given set of meta-modifiers.
    *
    * Useful to declare special handling for, e.g., `KeyEvent.KEYCODE_ENTER`.
    */

  def onKey(keyCode: Int, metaState: Int = 0)( func: => Unit ):Unit = {
    installOnKeyListener
    keyCodeHandlers += ((keyCode, metaState) -> (() => func))
  }

}

/** "JQuery-style" handler declarations for AdapterView-specific events. */

trait PositronicItemHandlers {

  def setOnItemClickListener( l: AdapterView.OnItemClickListener ): Unit
  def setOnItemLongClickListener( l: AdapterView.OnItemLongClickListener ): Unit

  /** Call `func` when there is a click on an item.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.  The handler can call
    * `getItemAtPosition` to get the item itself, if need be.
    */

  def onItemClick( func: (View, Int, Long) => Unit) = {
    setOnItemClickListener( new AdapterView.OnItemClickListener {
      def onItemClick( parent: AdapterView[_], view: View,
                       position: Int, id: Long ) = { 
        func( view, position, id ) 
      }
    })
  }
   
  /** Call `func` when there is a long click on an item.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.  The handler can call
    * `getItemAtPosition` to get the item itself, if need be.
    *
    * The framework is always told that the event was handled; use
    * `onItemLongClickMaybe` if your handler might want to decline
    * the event, and leave it to the framework.
    */

  def onItemLongClick( func: (View, Int, Long) => Unit) = {
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id ); return true
      }
    })
  }
   
  /** Call `func` when there is a long click on an item.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.
    *
    * Return value is `true` if the event was handled; `false` otherwise.
    */

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

  def setOnItemSelectedListener( l: AdapterView.OnItemSelectedListener ):Unit

  // The item selected listener stuff is particularly awkward, since
  // we've got two distinct event handlers bundled up in one listener
  // class...

  private var haveOnItemSelectedListener = false
  private var itemSelectedHandler:    (( View, Int, Long ) => Unit) = null
  private var nothingSelectedHandler: (View => Unit)                = null

  private def installOnItemSelectedListener = {
    if (!haveOnItemSelectedListener) {

      haveOnItemSelectedListener = true

      setOnItemSelectedListener( new AdapterView.OnItemSelectedListener {

        def onItemSelected( parent: AdapterView[_], view: View, 
                            position: Int, id: Long ) = {
          handleItemSelected( view, position, id )
        }

        def onNothingSelected( parent: AdapterView[_] ) = {
          handleNothingSelected
        }

      })
    }
  }

  /** Call `func` when an item is selected.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.  The handler can call
    * `getItemAtPosition` to get the item itself, if need be.
    */

  def onItemSelected( handler: (View, Int, Long) => Unit ) = {
    installOnItemSelectedListener
    itemSelectedHandler = handler
  }

  /** Call `func` when the selection vanishes from this view ---
    * either because "touch is activated" (quoth the platform docs)
    * or because the adapter is now empty.
    */

  def onNothingSelected( handler: View => Unit ) = {
    installOnItemSelectedListener
    nothingSelectedHandler = handler
  }

  private def handleItemSelected( view: View, posn: Int, id: Long ) =
    if (itemSelectedHandler != null) itemSelectedHandler( view, posn, id )

  private def handleNothingSelected =
    if (nothingSelectedHandler != null) nothingSelectedHandler

  /** Return the item relevant to the `ContextMenu` selection which gave
    * us this `ContextMenuInfo`.
    */

  def selectedContextMenuItem( info: ContextMenu.ContextMenuInfo ):Object = {
    val posn = info.asInstanceOf[ AdapterView.AdapterContextMenuInfo ].position
    return getItemAtPosition( posn )
  }
   
}

/** Mixin trait for android acitivities which provides extra hooks
  * for integrated lifecycle management.  Fortunately, these
  * don't conflict with the native API because they're alternate
  * overloadings.  Also includes jQuery-style handler declarations
  * for menu events, and similar conveniences.
  *
  * NB the Scaladoc here is more readable if inherited methods
  * are omitted, particularly as the inherited methods include the
  * entire standard `Activity` API, when most likely, the extensions
  * provided by the trait itself are the only things you'll want to
  * read about here.
  */

trait PositronicActivityHelpers
 extends _root_.android.app.Activity
{
  /** Run `handler` whenever the given
    * [[org.positronicnet.notifications.Notifier]] signals a change,
    * within the lifetime of this activity.
    * 
    * The `handler` is automatically run on the UI thread, even if notifications
    * come from somewhere else (e.g., a db thread).
    */

  def onChangeTo[T]( notifier: Notifier[T] )( handler: T => Unit ) = 
    manageListener( this, notifier )( handler )

 /** Run `handler` whenever the given
    * [[org.positronicnet.notifications.Notifier]] signals a change,
    * within the lifetime of this activity, and using `listener` as the
    * tag to unregister when the listener is destroyed.  Like `onChangeTo`,
    * but necessary if more than one listener might be listening to a
    * given notifier in service to this `Activity`.
    * 
    * The `handler` is automatically run on the UI thread, even if notifications
    * come from somewhere else (e.g., a db thread).
    */

  def manageListener[T]( listener: AnyRef, source: Notifier[T])( handler: T => Unit ) = {
    source.onChange( listener ){ notice =>
      this.runOnUiThread{ handler( notice ) }
    }

    this.onDestroy{ source.stopNotifier( listener ) }
  }

  /** Open the given [[org.positronicnet.facility.AppFacility]], and arrange
    * for it to be closed when this activity is destroyed.
    */

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

  protected [positronicnet]
  class Handlers extends ArrayBuffer[ () => Unit ] {
    def runAll = for (handler <- this) { handler() }
  }

  private var onCreateNotifier  = new Handlers
  private var onRestartNotifier = new Handlers
  private var onStartNotifier   = new Handlers
  private var onResumeNotifier  = new Handlers
  private var onPauseNotifier   = new Handlers
  private var onStopNotifier    = new Handlers
  private var onDestroyNotifier = new Handlers

  override def onCreate( b: Bundle ) {
    onCreate( b, 0 )
  }

  private [positronicnet]
  def onCreate( b: Bundle, layoutResourceId: Int ) = {
    super.onCreate( b )
    if (layoutResourceId != 0) { setContentView( layoutResourceId ) }
    if ( b != null ) recreateInstanceState( b )
    onCreateNotifier.runAll
  }

  /** Invoked as `onCreate{ ... body ... }`.
    *
    * Runs `body` (possibly among others) when the framework calls the
    * base `onCreate` method.
    */

  def onCreate( thunk: => Unit ) = { onCreateNotifier.append( () => thunk ) }

  override def onRestart = { 
    super.onRestart(); 
    onRestartNotifier.runAll
  }
  
  /** Invoked as `onRestart{ ... body ... }`.
    *
    * Runs `body` (possibly among others) when the framework calls the
    * base `onRestart method.
    */

  def onRestart( thunk: => Unit ) = { onRestartNotifier.append( () => thunk )}

  override def onResume = { 
    super.onResume(); 
    onResumeNotifier.runAll
  }
  
  /** Invoked as `onResume{ ... body ... }`.
    *
    * Runs `body` (possibly among others) when the framework calls the
    * base `onResume method.
    */

  def onResume( thunk: => Unit ) = { onResumeNotifier.append( () => thunk ) }

  override def onPause = { 
    super.onPause(); 
    onPauseNotifier.runAll
  }
  
  /** Invoked as `onPause{ ... body ... }`.
    *
    * Runs `body` (possibly among others) when the framework calls the
    * base `onPause` method.
    */

  def onPause( thunk: => Unit ) = { onPauseNotifier.append( () => thunk ) }

  override def onStop = { 
    super.onStop(); 
    onStopNotifier.runAll
  }
  
  /** Invoked as `onStop{ ... body ... }`.
    *
    * Runs `body` (possibly among others) when the framework calls the
    * base `onStop method.
    */

  def onStop( thunk: => Unit ) = { onStopNotifier.append( () => thunk ) }

  override def onDestroy = { 
    super.onDestroy(); 
    onDestroyNotifier.runAll
  }
  
  /** Invoked as `onDestroy{ ... body ... }`.
    *
    * Runs `body` (possibly among others) when the framework calls the
    * base `onDestroy method.
    */

  def onDestroy( thunk: => Unit ) = { onDestroyNotifier.append( () => thunk )}

  // Versions of onSaveInstanceState and friends which eliminate
  // the super.foo() noise, and only get called if there *is* a
  // bundle to unpack.

  /** Called `onSaveInstanceState`, after `super.onSaveInstanceState()`,
    * to save any extra state in the bundle.  As per the standard method,
    * except that it calls `super` to save the framework state automatically,
    * so you don't have to.
    */

  def saveInstanceState( b: Bundle ) = {}

  /** Called `onCreate`, to restore instance state from the `Bundle`, only
    * if a `Bundle` is supplied.
    */

  def recreateInstanceState( b: Bundle ) = {}

  /** Called `onRestoreInstanceState to restore instance state from
    * the `Bundle`, only if a `Bundle` is supplied.
    */

  def restoreInstanceState( b: Bundle ) = {}

  override def onSaveInstanceState( b: Bundle ) = {
    super.onSaveInstanceState( b )
    if ( b != null ) saveInstanceState( b )
  }

  override def onRestoreInstanceState( b: Bundle ) = {
    super.onRestoreInstanceState( b )
    if ( b != null ) restoreInstanceState( b )
  }

  /** Run the `thunk` on this activity's UI thread. */

  def runOnUiThread( thunk: => Unit ):Unit = {
    this.runOnUiThread( new Runnable {
      def run() = { thunk }
    })
  }

  // Shorthands for dealing with menus

  private var optionsMenuResourceId = 0
  private var contextMenuResourceId = 0

  /** Select a menu resource to inflate and use as our options menu.
    * Implements the common case of `onCreateOptionsMenu` with less chatter.
    */

  def useOptionsMenuResource( id: Int ) = optionsMenuResourceId = id

  /** Select a menu resource to inflate and use as our context menu.
    * Implements the common case of `onCreateOptionsMenu` with less chatter.
    */

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
  }

  private val optionsItemMap = new HashMap[ Int, (() => Unit) ]

  /** When the options menu item with the given resource `id` is
    * selected, run the `thunk`.
    */

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

  type ContextItemHandler = 
    (( ContextMenu.ContextMenuInfo, View ) => Unit )

  private val contextItemMap = new HashMap[ Int, ContextItemHandler ]
  private var contextMenuView: View = null

  private def rememberViewForContextMenu( v: View ) = { contextMenuView = v }

  /** When the context menu item with the given resource `id` is
    * selected, run the `handler`.
    *
    * The handler should take two arguments --- a `ContextMenu.ContextMenuInfo`
    * and the `View` on which the user selected the context menu.
    */

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

  private var onPrepareOptionsMenuHandlers = new ArrayBuffer[ Menu => Unit ]

  override def onPrepareOptionsMenu( m: Menu ):Boolean = { 
    super.onPrepareOptionsMenu( m ); 
    for( handler <- onPrepareOptionsMenuHandlers ) { handler( m ) }
    return true;
  }
  
  def onPrepareOptionsMenu( handler: Menu => Unit ) = 
    onPrepareOptionsMenuHandlers.append( handler )

  // And these, just to cut down on noise.

  /** Shorthand for making, and immediately showing, a toast */

  def toast( msgResId: Int, duration: Int = Toast.LENGTH_SHORT ):Unit =
    Toast.makeText( this, msgResId, duration ).show

  /** Shorthand for making, and immediately showing, a toast */

  def toastShort( msgResId: Int ):Unit =
    Toast.makeText( this, msgResId, Toast.LENGTH_SHORT ).show

  /** Shorthand for making, and immediately showing, a toast */

  def toastShort( msg: String ):Unit =
    Toast.makeText( this, msg,      Toast.LENGTH_SHORT ).show

  /** Shorthand for making, and immediately showing, a toast */

  def toastLong( msgResId: Int ):Unit =
    Toast.makeText( this, msgResId, Toast.LENGTH_LONG ).show

  /** Shorthand for making, and immediately showing, a toast */

  def toastLong( msg: String ):Unit =
    Toast.makeText( this, msg,      Toast.LENGTH_LONG ).show
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

/** An `android.widget.Button` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicButton( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Button( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.EditText` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicEditText( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.EditText( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.TextView` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicTextView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.TextView( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.ListView` with [[org.positronicnet.ui.PositronicHandlers]]
  * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
  */

class PositronicListView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.ListView( context, attrs ) 
 with PositronicHandlers 
 with PositronicItemHandlers

/** An `android.widget.Spinner` with [[org.positronicnet.ui.PositronicHandlers]]
  * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
  */

class PositronicSpinner( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Spinner( context, attrs ) 
 with PositronicHandlers 
 with PositronicItemHandlers

/** An `android.widget.ImageView` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicImageView( context: Context, attrs: AttributeSet = null )
 extends android.widget.ImageView( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.LinearLayout` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicLinearLayout( context: Context, attrs: AttributeSet = null )
 extends android.widget.LinearLayout( context, attrs ) 
 with PositronicHandlers

/** Shorthand `android.app.Dialog` class with some extra constructor
  * arguments as a convenience.
  */

class PositronicDialog( context: Context, 
                        theme: Int = 0, 
                        layoutResourceId: Int = 0 )
 extends android.app.Dialog( context, theme ) 
{
  if ( layoutResourceId != 0 )
    setContentView( layoutResourceId )
}

/** Shorthand `android.app.Activity` class with
  * [[org.positronicnet.ui.PositronicActivityHelpers]] mixed in, and
  * some extra constructor
  * arguments as a convenience.
  */

class PositronicActivity( layoutResourceId: Int = 0 )
 extends android.app.Activity
 with PositronicActivityHelpers 
{
  override def onCreate( b: Bundle ) = super.onCreate( b, layoutResourceId )
}

/** Adapter for cursors produced by PositronicDb queries.
  * Automatically handles a fair amount of the usual typecasting
  * gubbish...
  */
abstract class CursorSourceAdapter[T <: AnyRef]( 
  activity: PositronicActivityHelpers,
  converter: PositronicCursor => T,
  source: Notifier[PositronicCursor] = null,
  itemViewResourceId: Int = 0
)
 extends _root_.android.widget.CursorAdapter( activity, null )
{
  var inflater: LayoutInflater = null

  if (source != null) {
    activity.manageListener( this, source ) {
      this.changeCursor( _ )
    }
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

/**
  * Adapter for Scala `IndexedSeq`s.
  *
  * Supports `newView` and `bindView` methods, analogous to those
  * provided by the base framework's `CursorAdapter` (though
  * `newView` takes only the parent `ViewGroup` as an argument).
  *
  * Note that the `T <: Object` restriction is needed so that
  * our `getItem( _: Int ):T` is compatible with the declared
  * `getItem( _: Int ): java.lang.Object` in the Adapter interface.
  * So, if you really want an adapter for an `IndexedSeq[Long]`,
  * you're on your own.
  *
  * The `itemViewResourceId` and `itemTextResourceId` constructor
  * arguments are optional, but are used by the default implementations
  * of `newView` and `bindView`, q.v., to handle simple cases with a minimum
  * of extra code.
  */

class IndexedSeqAdapter[T <: Object](protected var seq:IndexedSeq[T] = new ArrayBuffer[T],
                                     itemViewResourceId: Int = 0, 
                                     itemTextResourceId: Int = 0
                                    ) 
  extends _root_.android.widget.BaseAdapter 
{
  protected var inflater: LayoutInflater = null

  /** Method to reset the sequence if a new copy was (or might have been)
    * loaded off the UI thread.
    */

  def resetSeq( newSeq: IndexedSeq[T] ) = {
    seq = newSeq
    notifyDataSetChanged
  }

  /** Get a view to use for the given position.  Ordinarily delegates to the
    * `newView` and `bindView` methods, q.v.
    */

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

  /** Create a new view to display items (if our `AdapterView`'s pool has
    * no spares).
    *
    * If it's not overridden, and if an `itemViewResourceId` was supplied
    * to the constructor, the default implementation will use a layout
    * inflater to inflate that resource, and return the result.
    */

  def newView( parent: ViewGroup ): View = {
    assert( itemViewResourceId != 0 )
    inflater.inflate( itemViewResourceId, parent, false )
  }

  /** Make one of the views resulting from `newView` display a particular
    * `item`.
    *
    * If it's not overridden, it will call `toString` on the item, and
    * try to stuff the resulting string into a relevant `TextView`.  If
    * an `itemTextResourceId` was supplied to the adapter constructor,
    * we'll call `findViewById` on the `view` we get to find the `View`
    * we update.  Otherwise, the `view` we get (returned by `newView`)
    * must be a `TextView`, and we'll update that.
    */

  def bindView( view: View, item: T ) = {
    val textView = 
      (if (itemTextResourceId != 0)
        view.findViewById( itemTextResourceId )
       else
         view).asInstanceOf[ android.widget.TextView ]

    textView.setText( item.toString )
  }

  /** Get the n'th item from the current sequence */

  def getItem(position: Int):T = seq(position)

  /** Get the id of the n'th item from the current sequence */

  def getItemId(position: Int) = getItem(position).hashCode()

  /** Get number of items in the current sequence */

  def getCount = seq.size
}

/**
  * Adapter for [[org.positronicnet.notifications.Notifier]]s which
  * manage (and report changes to) Scala `IndexedSeq`s.
  *
  * Like [[org.positronicnet.ui.IndexedSeqAdapter]], except that it wires
  * itself up to automatically be notified of changes within the lifetime
  * of the given `activity`.
  */

class IndexedSeqSourceAdapter[T <: Object](activity: PositronicActivityHelpers,
                                           source: Notifier[IndexedSeq[T]],
                                           itemViewResourceId: Int = 0, 
                                           itemTextResourceId: Int = 0 ) 
  extends IndexedSeqAdapter[T]( itemViewResourceId = itemViewResourceId,
                                itemTextResourceId = itemTextResourceId )
{
  activity.manageListener( this, source ) { resetSeq( _ ) }
}
