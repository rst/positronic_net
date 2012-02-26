package org.positronicnet.ui

import _root_.android.content.Context
import _root_.android.view.View
import _root_.android.view.Menu
import _root_.android.view.ContextMenu
import _root_.android.view.MenuItem
import _root_.android.os.Bundle
import _root_.android.widget.Toast
import _root_.android.util.Log

import org.positronicnet.facility.AppFacility
import org.positronicnet.notifications.Notifier

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

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
  // Note resource IDs associated with the R class in the package 
  // containing this activity (or its parents), if we haven't already...

  ResourceId.harvestAssociatedResources( this )

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

  /** If we're starting up (and not restoring instance state), this
    * method will be called at the same point in the lifecycle.
    *
    * (Technically, it actually gets invoked from onPostCreate, if no
    * "saved state" bundle is supplied, but before the superclass's
    * onPostCreate --- thus letting the framework's post-create
    * handling still run last, the way it expects.)
    */

  def createInstanceState = {}

  override def onPostCreate( b: Bundle ) = {
    if (b == null) createInstanceState
    super.onPostCreate( b )
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

