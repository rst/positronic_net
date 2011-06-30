package org.positronic.pubsub

import scala.collection.mutable.HashMap

trait ChangeNotifications {
  val changeHandlers = new HashMap[ AnyRef, () => Unit ]
  def onChange( key: AnyRef )( handlerThunk: => Unit ):Unit = {
    changeHandlers( key ) = (() => handlerThunk)
  }
  def stopChangeNotifications( key: AnyRef ):Unit = {
    changeHandlers.remove( key )
  }
  def noteChange = {
    for ((key, handler) <- changeHandlers) {
      handler()
    }
  }
}

class ChangeNotifier extends ChangeNotifications
