package org.positronicnet.notifications

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import org.positronicnet.facility._

// Requests to a notifier, to be delivered actor-style.

abstract class Action[T]

/** Base class for all notifier actions.  Not much use directly;
  * see subclasses.
  */

abstract class NotifierAction[T] extends Action[T]

/** Base class for all "query" actions, i.e., those yielding a future.
  * Not much use directly; see subclasses.
  *
  * Note that a `QueryAction[T,V]` is an action on a `Notifier[T]` returning
  * a `Future[V]`.
  */

abstract class QueryAction[T,V] extends NotifierAction[T]
{
  val complete: PartialFunction[ BaseNotifierImpl[T], V ]

  private [positronicnet]
  def doComplete( future: Future[V], notifier: BaseNotifierImpl[T] ) =
    try { future.succeed( complete( notifier ) ) }
    catch { case t: Throwable => future.fail( t ) }
}

/** Actions that can be sent to [[org.positronicnet.notifications.Notifer]]s.
  *
  * Typical syntax is `notifier ! action` (to have the action performed
  * on a background thread) or `notifier.onThisThread(action)` (to have
  * the action performed on the current thread).
  *
  * Many actions include a `handler` argument, which is a function
  * that takes a notifier update as an argument.  If the action is
  * invoked as `notifier ! action`, it's assumed that the caller is
  * on an Android `HandlerThread`; execution of the `handler` will
  * be posted back to the `HandlerThread`.  (This means that, in
  * particular, if the caller is an application's main UI thread,
  * it is safe to manipulate `View`s from within the `handler`.) 
  */

object Actions {

  /** Action to add a watcher that gets notified when
    * the state controlled by the notifier changes.
    * The `handler` is called on changes.
    *
    * If `AddWatcher` is sent as `notifier ! AddWatcher...`,
    * the assumption is that the message is being sent from
    * an Android `HandlerThread` (typically, an application's
    * main "UI thread"), and invocations of the `handler` are
    * posted back to that thread, which makes it safe for a
    * `handler` to manipulate `View` objects, and so forth.
    *
    * The `key` is anything that can be used as a hash key;
    * it is used in a subsequent `StopWatcher` call, to
    * cease notifications.
    */

  def AddWatcher[T]( key: AnyRef )( handler: T => Unit ) =
    AddWatcherAction( key, handler )

  /** A combination of `AddWatcher` and `Fetch`, with the same `handler`
    * used for both.
    */

  def AddWatcherAndFetch[T]( key: AnyRef )( handler: T => Unit ) =
    AddWatcherAndFetchAction( key, handler )

  /** Query action --- gives a future for the current value of the
    * notifier.
    */

  def Query[T] = new DoQuery[T]

  private [notifications]
  class DoQuery[T] extends QueryAction[T,T] {
    val complete: PartialFunction[BaseNotifierImpl[T], T] = {
      case n: BaseNotifierImpl[T] =>
        n.fetchOnThisThread
    }
  }

  /** Give a current snapshot of the notifier's state to the `handler`.
    * Consider using `Query` instead.
    */

  case class Fetch[T]( handler: T => Unit ) 
    extends NotifierAction[T]

  /** Cease notifications to a watcher added by `AddWatcher` */

  case class StopWatcher[T]( key: AnyRef ) 
    extends NotifierAction[T]

  private [notifications]
  case class AddWatcherAction[T]( key: AnyRef, handler: T => Unit ) 
    extends NotifierAction[T]

  private [notifications]
  case class AddWatcherAndFetchAction[T]( key: AnyRef, handler: T => Unit ) 
    extends NotifierAction[T]

  /** Change the query for a [[org.positronicnet.notifications.NotifierWithQuery]].
    *
    * This is a change which will cause any watchers to get an update.
    */

  case class Requery[Q, R]( newQuery: Q ) 
    extends NotifierAction[R]
}

import Actions._

/** Basic trait for actor-like objects which manage state of type `T`.
  * Actors ordinarily operate on a thread associated with the state
  * they manage (e.g., the worker thread associated with a
  * [[org.positronicnet.db.Database]]).  Code running on other threads
  * usually communicates with them by sending `action`s using an
  * actor-like `notifier ! action` style.  
  *
  * The base `Notifier` trait specifies a set of actions for querying
  * the state, and requesting updates when it changes.  Subclasses
  * (e.g., [[org.positronicnet.orm.RecordManager]]) typically support
  * other actions which request changes to happen (which may, in turn,
  * cause watchers to be notified).
  *
  * The primary reason for doing things in an actor-style syntax in
  * Android is to deal with the fairly stringent best-practice requirements
  * for Android "user interface threads" --- specifically, the requirement
  * that they shouldn't be waiting for anything else, even local disk I/O
  * (let alone, say, a network operation).  So, all that stuff needs to
  * happen on background threads; we use actor style to help keep the
  * concurrency manageable.
  *
  * However, there are also circumstances where concurrency is a nuisance.
  * For instance, an Android `Service` or `BroadcastReceiver` may want to
  * be sure that an operation has completed before signalling that it has
  * finished itself.
  *
  * For these situations, you can also request that an action be performed
  * `onThisThread`, as in
  *
  *   `notifier.onThisThread( Fetch{ value => ... } )`
  *
  * which performs the `Fetch` and runs the body on the current thread,
  * forgoing all concurrency.  (Though for this particular case, there's
  * a special-case `fetchOnThisThread` method which simply fetches the
  * monitored state and returns it as the value.)
  *
  * See [[org.positronicnet.notifications.Actions]] for definitions of the
  * common action types.
  */

trait Notifier[T] {

  // "Action" interface

  /** Perform the `action`, usually on a worker thread associated with this
    * notifier.  Results may be posted back to the calling thread, viz.
    * the conventions documented in [[org.positronicnet.notifications.Actions]].
    */

  def !( action: Action[T] ): Unit

  /** Perform the `action`, synchronously on the calling thread. 
    */

  def onThisThread( action: Action[T] ): Unit

  /** Synchronously fetch the value being monitored by this `Notifier`. */

  def fetchOnThisThread: T

  // Older interface, still used by the UI code...

  private[positronicnet]
  def watch( key: AnyRef )( handler: T => Unit ): Unit

  private[positronicnet]
  def fetch( handler: T => Unit ): Unit

  private[positronicnet]
  def fetchAndWatch( key: AnyRef )( handler: T => Unit ): Unit

  private[positronicnet]
  def onChange( key: AnyRef )( handler: T => Unit ): Unit

  private[positronicnet]
  def stopNotifier( key: AnyRef ): Unit
}

/** Machinery for Notifiers which just delegate to some other Notifier. */

trait NotifierDelegator[T] extends Notifier[T] {

  /** The Notifier to which the delegator forwards, well... everything here. */

  def notifierDelegate: Notifier[T]

  def fetchOnThisThread = notifierDelegate.fetchOnThisThread

  def !( action: Action[T] ) = notifierDelegate ! action

  def onThisThread( action: Action[T] ) = 
    notifierDelegate.onThisThread( action )

  private[positronicnet]
  def watch( key: AnyRef )( handler: T => Unit ) = 
    notifierDelegate.watch( key )( handler )

  private[positronicnet]
  def fetch( handler: T => Unit ) = notifierDelegate.fetch( handler )

  private[positronicnet]
  def fetchAndWatch( key: AnyRef )( handler: T => Unit ) = 
    notifierDelegate.fetchAndWatch( key )( handler )

  private[positronicnet]
  def onChange( key: AnyRef )( handler: T => Unit ) = 
    notifierDelegate.onChange( key )( handler )

  private[positronicnet]
  def stopNotifier( key: AnyRef ) = notifierDelegate.stopNotifier( key )
}

/** Base class for all library Notifiers. */

trait NotifierImpl[T] extends Notifier[T] {

  // Interface used internally to trigger notifications.

  def noteChange: Unit
}

/** Trait providing common machinery for all library
  * [[org.positronicnet.notifications.Notifier]] classes.
  */

trait BaseNotifierImpl[T] extends NotifierImpl[T] {
  protected val changeHandlers = new HashMap[ AnyRef, T => Unit ]

  def onThread( thunk: => Unit ): Unit
  def fetchOnThisThread = currentValue

  private def wrapHandler( handler: T => Unit ): T => Unit = 
    CallbackManager.wrapHandler( handler )

  def ?[V]( action: QueryAction[T,V] ) = {
    val future = new Future[V]
    onThread{ action.doComplete( future, this ) }
    future
  }

  def !( action: Action[T] ): Unit = 
    action match {
      case Fetch( handler ) => {
        val wrapped = wrapHandler( handler )
        onThread{ wrapped( currentValue ) }
      }
      case AddWatcherAction( key, handler ) => {
        watch( key )( wrapHandler( handler ))
      }
      case AddWatcherAndFetchAction( key, handler ) => {
        this ! Fetch( handler )
        this ! AddWatcherAction( key, handler )
      }
      case StopWatcher( key ) => stopNotifier( key )
      case _ => onThread{ onThisThread( action ) }
    }

  def onThisThread( action: Action[T] ): Unit =
    action match {
      case Fetch( handler ) => handler( currentValue )
      case _ => 
        throw new IllegalArgumentException( "Unrecognized action: " + 
                                            action.toString )
    }

  private[positronicnet]
  def watch( key: AnyRef )( handler: T => Unit ): Unit = {
    changeHandlers( key ) = handler
  }

  private[positronicnet]
  def fetch( handler: T => Unit ): Unit = {
    onThread{ handler( currentValue ) }
  }

  private[positronicnet]
  def fetchAndWatch( key: AnyRef )( handler: T => Unit ) = {
    fetch( handler )
    watch( key )( handler )
  }

  private[positronicnet]
  def onChange( key: AnyRef )( handler: T => Unit ):Unit = {
    fetchAndWatch( key )( handler )
  }
  private[positronicnet]
  def stopNotifier( key: AnyRef ):Unit = {
    changeHandlers.remove( key )
  }
  def noteChange: Unit
  protected def currentValue: T
}

// Dealing with Android's "Handler" machinery for arranging
// callbacks on application main threads

protected[positronicnet] object CallbackManager
  extends ThreadLocal[ android.os.Handler ]
{
  def wrapHandler[T]( handler: T => Unit ): T => Unit = {
    val cbManager = CallbackManager.forThisThread
    (( v: T ) => {
      cbManager.post( new Runnable{ 
        override def run = { handler( v ) }
      })
    })
  }

  def wrapPartialHandler[T,V]( handler: PartialFunction[T,V] ): T => Unit = {
    val cbManager = CallbackManager.forThisThread
    (( v: T ) => {
      cbManager.post( new Runnable{ 
        override def run = { 
          if (handler.isDefinedAt(v))
            handler( v ) 
        }
      })
    })
  }

  def forThisThread: android.os.Handler = {
    var valueNow = this.get
    if (valueNow == null) {
      valueNow = new android.os.Handler
      this.set( valueNow )
    }
    return valueNow
  }
}

/** Notifier machinery which sends a separate copy of its state to each watcher.
  *
  * Appropriate when the state is mutable (such as a Cursor), and watchers
  * who were all trying to use it (e.g., Fragments presenting different
  * visualizations of the same query result) could interfere with each
  * other if they tried to share state (e.g., by one changing a Cursor's
  * current row behind the other's back).
  */

trait NonSharedNotifier[T]
  extends BaseNotifierImpl[T]
{
  def noteChange = {
    for ((key, handler) <- changeHandlers) {
      handler( currentValue )           // fresh copy for each listener
    }
  }
  protected def currentValue: T
}

/** Notifier machinery which sends the same copy of its state to all watchers.
  *
  * Appropriate when the state is immutable, and all watchers can share it
  * without stepping on each others' toes.
  */

trait CachingNotifier[T]
  extends BaseNotifierImpl[T]
{
  protected var cachedValue: T = currentValue

  def noteChange = {
    if (!changeHandlers.isEmpty) {
      cachedValue = currentValue
      for ((key, handler) <- changeHandlers) {
        handler( cachedValue )           // one copy for all listeners
      }
    }
  }
}

// Machinery for change listeners which also depend on query
// parameters from the UI...

/** Subclass of [[org.positronicnet.notifications.Notifier]] which
  * typically represents a query on some underlying resource.
  *
  * The query has a query type, `Q`, and a result type, `R`.
  * Constructors typically take an initial query (of type `Q`) as
  * an argument.  The `Requery` action can be used to change the
  * query; this is a change that causes all currently registered
  * watchers (viz. `AddWatcher` in [[org.positronicnet.notifications.Actions]])
  * to get notified of the new results.
  */

trait NotifierWithQuery[ Q, R ]
  extends BaseNotifierImpl[ R ]
{
  protected var currentQuery: Q

  def currentParams: Q = currentQuery

  override def onThisThread( action: Action[R] ): Unit =
    action match {
      case r: Requery[Q,R] => requery( r.newQuery )
      case _ => super.onThisThread( action )
    }

  def requery( q: Q ) = {
    currentQuery = q; noteChange
  } 
}

/** Base class providing useful common machinery for all stock
  * [[org.positronicnet.notifications.Notifier]] classes
  */

class BaseNotifier( facility: AppFacility )
{
  def onThread( thunk: => Unit ) = { 
    facility match {
      case w: WorkerThread => w.runOnThread{ thunk }
      case _ => thunk
    }
  }
}

/** Notifier class which sends the same copy of its state to all watchers.
  *
  * Appropriate when the state is immutable, and all watchers can share it
  * without stepping on each others' toes.
  */

class ValueNotifier[T]( facility: AppFacility, generator: () => T )
  extends BaseNotifier( facility )
  with CachingNotifier[T]
{
  protected def currentValue = generator()
}

/** Notifier class which sends a separate copy of its state to each watcher.
  *
  * Appropriate when the state is mutable (such as a Cursor), and watchers
  * who were all trying to use it (e.g., Fragments presenting different
  * visualizations of the same query result) could interfere with each
  * other if they tried to share state (e.g., by one changing a Cursor's
  * current row behind the other's back).
  */

class NonSharedValueNotifier[T]( facility: AppFacility, generator: () => T )
  extends BaseNotifier( facility )
  with NonSharedNotifier[T]
{
  protected def currentValue = generator()
}

/** "Query" class which sends the same copy of its state to all watchers.
  * See [[org.positronicnet.notifications.NotifierWithQuery]].
  *
  * Appropriate when the state is immutable, and all watchers can share it
  * without stepping on each others' toes.
  */

class ValueQuery[Q, R]( facility: AppFacility, 
                        initialQuery: Q, queryFunc: Q => R )
  extends BaseNotifier( facility )
  with NotifierWithQuery[ Q, R ]
{
  protected var currentQuery: Q = initialQuery
  protected var cachedValue: R = currentValue

  protected def currentValue: R = queryFunc( currentQuery )

  def noteChange = {
    if (!changeHandlers.isEmpty) {
      cachedValue = currentValue
      for ((key, handler) <- changeHandlers) {
        handler( cachedValue )           // one copy for all listeners
      }
    }
  }
}

/** "Query" class which sends a separate copy of its state to each watcher.
  * See [[org.positronicnet.notifications.NotifierWithQuery]].
  *
  * Appropriate when the state is mutable (such as a Cursor), and watchers
  * who were all trying to use it (e.g., Fragments presenting different
  * visualizations of the same query result) could interfere with each
  * other if they tried to share state (e.g., by one changing a Cursor's
  * current row behind the other's back).
  */

class NonSharedValueQuery[Q, R]( facility: AppFacility,
                                 initialQuery: Q, queryFunc: Q => R )
  extends BaseNotifier( facility )
  with NotifierWithQuery[ Q, R ]
  with NonSharedNotifier[ R ]
{
  protected var currentQuery: Q = initialQuery
  protected def currentValue = queryFunc( currentQuery )
}

/** Interface for an object which manages multiple notifiers.
  *
  * It's common to have a set of [[org.positronicnet.notifications.Notifier]]s
  * for different aspects of a single underlying state.
  */

trait NotificationManager
{
  def onThread( thunk: => Unit ): Unit
  def doChange( thunk: => Unit ): Unit
  def noteChange: Unit

  def valueNotifier[T]( thunk: => T ): CachingNotifier[T]
  def cursorNotifier[T]( thunk: => T ): Notifier[T]

  def valueQuery[Q,R]( initialVal: Q )( func: Q => R ): ValueQuery[Q, R]
  def cursorQuery[Q,R]( initialVal: Q )( func: Q => R ):NonSharedValueQuery[Q,R]
}

/** Basic machinery for all library
  * [[org.positronicnet.notifications.NotificationManager]]s
  */

abstract class BaseNotificationManager( facility: AppFacility )
  extends BaseNotifier( facility )
  with NotificationManager
{
  private val notifiers = new ArrayBuffer[ NotifierImpl[_] ]

  // Wrapper for domain operations:  we run on the facility's
  // worker thread if there is one, and notify the listeners when
  // done.

  def doChange( thunk: => Unit ) = onThread{ thunk; noteChange }

  def noteChange = notifiers.foreach{ _.noteChange }

  def valueNotifier[T](thunk: => T): CachingNotifier[T] = {
    val it = new ValueNotifier( facility, () => thunk )
    notifiers += it
    return it
  }

  def cursorNotifier[T](thunk: => T): Notifier[T] = {
    val it = new NonSharedValueNotifier( facility, () => thunk )
    notifiers += it
    return it
  }

  def valueQuery[Q,R]( initialVal: Q)(queryFunc: Q => R ): ValueQuery[Q, R] = {
    val it = new ValueQuery( facility, initialVal, queryFunc )
    notifiers += it
    return it
  }

  def cursorQuery[Q,R]( initialVal: Q)(queryFunc: Q => R ): NonSharedValueQuery[Q, R] = {
    val it = new NonSharedValueQuery( facility, initialVal, queryFunc )
    notifiers += it
    return it
  }
}

/** Support for [[org.positronicnet.notifcations.NotificationManager]]s
  * which delegate their work to another, pre-existing one.
  */

abstract class BaseNotificationDelegator[ T <: NotificationManager ]( d: T )
{
  protected val notificationManagerDelegate: T = d

  def onThread( thunk: => Unit ) = d.onThread( thunk )
  def doChange( thunk: => Unit ) = d.doChange( thunk )
  def noteChange = d.noteChange

  def valueNotifier[T]( thunk: => T )  = d.valueNotifier( thunk )
  def cursorNotifier[T]( thunk: => T ) = d.cursorNotifier( thunk )

  def valueQuery[Q,R]( initialVal: Q )( func: Q => R ) = 
    d.valueQuery( initialVal )( func )

  def cursorQuery[Q,R]( initialVal: Q )( func: Q => R ) =
    d.cursorQuery( initialVal )( func )
}

