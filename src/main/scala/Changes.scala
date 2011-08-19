package org.positronicnet.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

// Basic definition of something that can notify watchers about
// an object of type T.

trait Notifier[T] {
  protected val changeHandlers = new HashMap[ AnyRef, T => Unit ]

  def onThread( thunk: => Unit ): Unit
  def fetchOnThisThread = currentValue

  private def wrapHandler( handler: T => Unit ) = {
    val cbManager = CallbackManager.forThisThread
    (( v: T ) => {
      cbManager.post( new Runnable{ 
       override def run = { handler( v ) }
     }).asInstanceOf[Unit]
    })
  }

  def !( action: Action[T] ): Unit = 
    action match {
      case Fetch( handler ) => {
        val wrapped = wrapHandler( handler )
        onThread{ wrapped( currentValue ) }
      }
      case AddWatcher( key, handler ) => {
        watch( key )( wrapHandler( handler ))
      }
      case AddWatcherAndFetch( key, handler ) => {
        this ! Fetch( handler )
        this ! AddWatcher( key, handler )
      }
      case StopWatching( key ) => stopNotifier( key )
      case _ => onThread{ onThisThread( action ) }
    }

  def onThisThread( action: Action[T] ): Unit =
    action match {
      case Fetch( handler ) => handler( currentValue )
      case _ => 
        throw new IllegalArgumentException( "Unrecognized action: " + 
                                            action.toString )
    }

  def watch( key: AnyRef )( handler: T => Unit ): Unit = {
    changeHandlers( key ) = handler
  }

  def fetch( handler: T => Unit ): Unit = {
    onThread{ handler( currentValue ) }
  }

  def fetchAndWatch( key: AnyRef )( handler: T => Unit ) = {
    fetch( handler )
    watch( key )( handler )
  }

  def onChange( key: AnyRef )( handler: T => Unit ):Unit = {
    fetchAndWatch( key )( handler )
  }
  def stopNotifier( key: AnyRef ):Unit = {
    changeHandlers.remove( key )
  }
  def noteChange: Unit
  protected def currentValue: T
}

// Requests a notifier, to be delivered actor-style.

abstract class Action[T]
abstract class NotifierAction[T] extends Action[T]

case class Fetch[T]( handler: T => Unit ) 
  extends NotifierAction[T]
case class AddWatcher[T]( key: AnyRef, handler: T => Unit ) 
  extends NotifierAction[T]
case class StopWatching[T]( key: AnyRef ) 
  extends NotifierAction[T]
case class AddWatcherAndFetch[T]( key: AnyRef, handler: T => Unit ) 
  extends NotifierAction[T]

// Dealing with Android's "Handler" machinery for arranging
// callbacks on application main threads

protected[positronicnet] object CallbackManager
  extends ThreadLocal[ android.os.Handler ]
{
  def forThisThread: android.os.Handler = {
    var valueNow = this.get
    if (valueNow == null) {
      valueNow = new android.os.Handler
      this.set( valueNow )
    }
    return valueNow
  }
}

// Form of change notification in which each listener gets its
// own private status update --- as when it's a cursor, which
// different listeners *can't* share without stomping all over
// each other.

trait NonSharedNotifier[T]
  extends Notifier[T]
{
  def noteChange = {
    for ((key, handler) <- changeHandlers) {
      handler( currentValue )           // fresh copy for each listener
    }
  }
  protected def currentValue: T
}

// Form of change notification in which the notifier computes
// one new value, which all listeners share (and which can be
// interrogated on the fly):

trait CachingNotifier[T]
  extends Notifier[T]
{
  protected var cachedValue: T = currentValue

  def value: T = cachedValue

  def noteChange = {
    cachedValue = currentValue
    for ((key, handler) <- changeHandlers) {
      handler( cachedValue )           // one copy for all listeners
    }
  }
}

// Machinery for change listeners which also depend on query
// parameters from the UI...

trait NotifierWithQuery[ Q, R ]
  extends Notifier[ R ]
{
  protected var currentQuery: Q

  def requery( q: Q ) = {
    currentQuery = q; noteChange
  } 
}

// Useful common machinery for classes managing changes to some
// resource (e.g., a database) which might maintain multiple
// change listeners.

class BaseNotifier( facility: AppFacility )
{
  def onThread( thunk: => Unit ) = { 
    facility match {
      case w: WorkerThread => w.runOnThread{ thunk }
      case _ => thunk
    }
  }
}

class ValueStream[T]( facility: AppFacility, generator: () => T )
  extends BaseNotifier( facility )
  with CachingNotifier[T]
{
  def currentValue = generator()
}

class NonSharedValueStream[T]( facility: AppFacility, generator: () => T )
  extends BaseNotifier( facility )
  with NonSharedNotifier[T]
{
  def currentValue = generator()
}

class ValueQuery[Q, R]( facility: AppFacility, 
                        initialQuery: Q, queryFunc: Q => R )
  extends BaseNotifier( facility )
  with NotifierWithQuery[ Q, R ]
  with CachingNotifier[ R ]
{
  protected var currentQuery: Q = initialQuery
  def currentValue = queryFunc( currentQuery )
}

class NonSharedValueQuery[Q, R]( facility: AppFacility,
                                 initialQuery: Q, queryFunc: Q => R )
  extends BaseNotifier( facility )
  with NotifierWithQuery[ Q, R ]
  with NonSharedNotifier[ R ]
{
  protected var currentQuery: Q = initialQuery
  def currentValue = queryFunc( currentQuery )
}

abstract class ChangeManager( facility: AppFacility )
  extends BaseNotifier( facility )
{
  private val notifiers = new ArrayBuffer[ Notifier[_] ]

  // Wrapper for domain operations:  we run on the facility's
  // worker thread if there is one, and notify the listeners when
  // done.

  def doChange( thunk: => Unit ) = onThread{ thunk; noteChange }

  def addSubNotifier[T]( notifier: Notifier[T] ) = 
    notifiers += notifier

  def noteChange = notifiers.foreach{ _.noteChange }

  def valueStream[T](thunk: => T): CachingNotifier[T] = {
    val it = new ValueStream( facility, () => thunk )
    notifiers += it
    return it
  }

  def cursorStream[T](thunk: => T): Notifier[T] = {
    val it = new NonSharedValueStream( facility, () => thunk )
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
