package org.positronicnet.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait ChangeNotifications[T] {
  protected val changeHandlers = new HashMap[ AnyRef, T => Unit ]
  def onChange( key: AnyRef )( handler: T => Unit ):Unit = {
    changeHandlers( key ) = handler
    handler( currentValue )
  }
  def stopChangeNotifications( key: AnyRef ):Unit = {
    changeHandlers.remove( key )
  }
  def noteChange: Unit
  protected def currentValue: T
}

// Form of change notification in which each listener gets its
// own private status update --- as when it's a cursor, which
// different listeners *can't* share without stomping all over
// each other.

trait NonSharedChangeNotifications[T]
  extends ChangeNotifications[T]
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

trait CachingChangeNotifications[T]
  extends ChangeNotifications[T]
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

abstract class NotifierWithQuery[ Q, R ]( initialQuery: Q )
  extends ChangeNotifications[ R ]
{
  protected var currentQuery = initialQuery

  def requery( q: Q ) = {
    currentQuery = q; noteChange
  } 
}

// Useful common machinery for classes managing changes to some
// resource (e.g., a database) which might maintain multiple
// change listeners.

class ValueStream[T]( generator: () => T )
  extends CachingChangeNotifications[T]
{
  def currentValue = generator()
}

class NonSharedValueStream[T]( generator: () => T )
  extends NonSharedChangeNotifications[T]
{
  def currentValue = generator()
}

class ValueQuery[Q, R]( initialQuery: Q, queryFunc: Q => R )
  extends NotifierWithQuery[ Q, R ]( initialQuery )
  with CachingChangeNotifications[ R ]
{
  def currentValue = queryFunc( currentQuery )
}

class NonSharedValueQuery[Q, R]( initialQuery: Q, queryFunc: Q => R )
  extends NotifierWithQuery[ Q, R ]( initialQuery )
  with NonSharedChangeNotifications[ R ]
{
  def currentValue = queryFunc( currentQuery )
}

abstract class ChangeManager( facility: AppFacility )
{
  private val notifiers = new ArrayBuffer[ ChangeNotifications[_] ]

  // Wrapper for domain operations:  we run on the facility's
  // worker thread if there is one, and notify the listeners when
  // done.

  def doChange( thunk: => Unit ) = { 
    facility match {
      case w: WorkerThread => w.runOnThread{ thunk; noteChange }
      case _ => thunk; noteChange
    }
  }

  def addSubNotifier[T]( notifier: ChangeNotifications[T] ) = 
    notifiers += notifier

  def noteChange = notifiers.foreach{ _.noteChange }

  def valueStream[T](thunk: => T): CachingChangeNotifications[T] = {
    val it = new ValueStream( () => thunk )
    notifiers += it
    return it
  }

  def cursorStream[T](thunk: => T): ChangeNotifications[T] = {
    val it = new NonSharedValueStream( () => thunk )
    notifiers += it
    return it
  }

  def valueQuery[Q,R]( initialVal: Q)(queryFunc: Q => R ): ValueQuery[Q, R] = {
    val it = new ValueQuery( initialVal, queryFunc )
    notifiers += it
    return it
  }

  def cursorQuery[Q,R]( initialVal: Q)(queryFunc: Q => R ): NonSharedValueQuery[Q, R] = {
    val it = new NonSharedValueQuery( initialVal, queryFunc )
    notifiers += it
    return it
  }
}
