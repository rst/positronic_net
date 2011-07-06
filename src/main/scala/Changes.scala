package org.positronicnet.util

import scala.collection.mutable.HashMap

trait ChangeNotifications[T] {
  val changeHandlers = new HashMap[ AnyRef, T => Unit ]
  def onChange( key: AnyRef )( handler: T => Unit ):Unit = {
    changeHandlers( key ) = handler
  }
  def stopChangeNotifications( key: AnyRef ):Unit = {
    changeHandlers.remove( key )
  }
  def noteChange( datum: T ) = {
    for ((key, handler) <- changeHandlers) {
      handler( datum )
    }
  }
  def noteChangeEach( thunk: => T ) = {
    for ((key, handler) <- changeHandlers) {
      handler( thunk )
    }
  }
}

class ChangeNotifier[T] extends ChangeNotifications[T]

