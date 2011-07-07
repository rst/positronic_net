package org.positronicnet.util

import scala.collection.mutable.HashMap

trait ChangeNotifications[T] {
  val changeHandlers = new HashMap[ AnyRef, T => Unit ]
  def onChange( key: AnyRef )( handler: T => Unit ):Unit = {
    changeHandlers( key ) = handler
    handler( currentValue )
  }
  def stopChangeNotifications( key: AnyRef ):Unit = {
    changeHandlers.remove( key )
  }
  def noteChange = {
    for ((key, handler) <- changeHandlers) {
      handler( currentValue )           // fresh copy for each listener
    }
  }
  protected def currentValue: T
}

abstract class ChangeNotifier[T] extends ChangeNotifications[T]


