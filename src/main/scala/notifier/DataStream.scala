package org.positronicnet.notifications

import scala.collection.mutable.HashMap

/** A DataStream represents a stream of data of some type T, which will
  * be periodically relayed to listeners which have declared an interest,
  * via the `withValue` method.  These are frequently the result of the
  * `??` stream-query operator.  (DataStreams can be built from other
  * DataStreams using the usual monadic combinators.)1
  *
  * Note that a listener may want to declare interest in a data stream
  * only intermittently --- while, for instance, a particular Android
  * Activity is active.  The way we represent these sequences is with
  * a `Duration`, which by definition is a `DataStream[DurationEvent]`
  * where `DurationEvent` has the two distinguished values `DurationStart`
  * and `DurationStop`.  
  *
  * So, if you want a DataStream that's only active during a particular
  * duration, you can do something like this:
  * 
  * {{{
  *    val myStream: DataStream[ Whatever ] = ...
  *    val fooActive: Duration = ...
  *    val selectiveStream = myStream.during( fooActive )
  * 
  *    selectiveStream.withValues{ value => ... }
  * }}}
  *
  * The withValues handler will be called whenever the `fooAction` sends
  * a `DurationStart`.  (This is in addition to the courtesy "current
  * value" that you get when calling `withValues` for the first time.)
  *
  * This facility is implemented in terms of underlying `addListener`
  * and `stopListener` functionality, which are also available for use
  * directly.
  */

trait DataStream[T] 
{
  /** Most recent value of this DataStream */

  def currentValue: T
  protected var cachedCurrent = currentValue

  /** Called by the implementation to notify whoever's listening to the
    * stream that a new value is available.
    */

  protected def notifyNewValue: Unit = {
    this.cachedCurrent = currentValue
    for (listener <- listeners.values)
      listener( this.cachedCurrent )
  }

  /** Receive this stream's current value, and further values from this
    * stream as they become available, due changes in the underlying
    * resources.
    *
    * If caller is on an Android `HandlerThread`, values will be posted
    * back to the same thread.
    */

  def withValues( handler: T => Unit ): Unit = {
    handler( currentValue )
    addListener( new Object, handler )  // dummy tag
  }

  /** Return a version of this data stream which will signal new values
    * only during the given Duration.  (Also, whenever the Duration signals
    * a start, all of our listeners will be notified of the current value,
    * in case it's changed while we were inactive.)
    */

  def during( d: Duration ): DataStream[T] = 
    new DurationFilteredDataStream( d, this )

  /** We're a stream of Ts.  This returns a stream of Vs, which depends
    * on our current value.  When we get a new value, `func` is reinvoked
    * to get a new stream of Vs, and the listeners to the stream of Vs
    * will see the result of that.
    */

  def flatMap[V]( func: T => DataStream[V] ) =
    new FlatMapProxyStream( func, this )

  /** We're a stream of Ts.  This returns a stream of Vs, which is always
    * the result of `func` applied to our current value.
    */

  def map[V]( func: T => V ) = new MapProxyStream( func, this )

  /** We're a stream of Ts.  This returns a stream which will notify listeners
    * only if the new value matches the predicate:
    */

  def filter[V]( pred: T => Boolean ) = new FilterProxyStream( pred, this )

  /** Add the handler as a listener to this DataStream, associated with a
    * tag that allows it to be removed later.
    *
    * If caller is on an Android `HandlerThread`, values will be posted
    * back to the same thread.
    */

  def addListener( tag: AnyRef, handler: T => Unit ) =
    listeners( tag ) = CallbackManager.wrapHandler( handler )

  /** Remove the listener associated with the given tag */

  def removeListener( tag: AnyRef ) = listeners.remove( tag )
    
  private[this] var listeners = new HashMap[ AnyRef, T => Unit ]
}

trait Duration extends DataStream[DurationEvent]

class DurationEvent
case object DurationStart extends DurationEvent
case object DurationStop extends DurationEvent

private class DurationFilteredDataStream[T]( duration: Duration,
                                             underlying: DataStream[T] )
  extends DataStream[T]
{
  def currentValue = underlying.currentValue

  duration.addListener ( this,  _ match {

    case DurationStart =>
      this.notifyNewValue
      underlying.addListener( this, dummy => this.notifyNewValue )

    case DurationStop =>
      underlying.removeListener( this )
  })
}

private[notifications] 
class FlatMapProxyStream[T,V]( mapFunc: T => DataStream[V],
                               underlying: DataStream[T] ) 
  extends DataStream[V]
{
  private[this] var vstream = mapFunc( underlying.currentValue )

  def currentValue = vstream.currentValue

  vstream.addListener( this, dummy => this.notifyNewValue )

  underlying.addListener( this, newVal => {
    vstream.removeListener( this )
    vstream = mapFunc( newVal )
    vstream.addListener( this, dummy => this.notifyNewValue)
  })
}

private[notifications] 
class MapProxyStream[T,V]( mapFunc: T => V,
                           underlying: DataStream[T] ) 
  extends DataStream[T]
{
  def currentValue = underlying.currentValue
  underlying.addListener( this, dummy => this.notifyNewValue )
}

private[notifications] 
class FilterProxyStream[T,V]( pred: T => Boolean,
                              underlying: DataStream[T] ) 
  extends DataStream[T]
{
  def currentValue = underlying.currentValue
  underlying.addListener( this, 
    newVal => if (pred(newVal)) this.notifyNewValue)
}
