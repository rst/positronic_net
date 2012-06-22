package org.positronicnet.notifications

import scala.collection.mutable.HashMap

/** Trait that indicates that an object can be queried for DataStreams.
  * Details left to the implementation of that object, but this trait
  * at least establishes a notational convention.
  */

trait StreamQueryable[T] {
  def ??[V]( action: StreamQuery[T,V] ): DataStream[V]
}

/** Trait for objects meant to be sent as queries to a StreamQueryable */

trait StreamQuery[T,V]

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
  /** A future for the first value of this DataStream, as of the time
    * of its creation.
    *
    * If the implementation only calls `noteNewValue` and never calls
    * `noteNewFuture` deriving from ExplicitNotificationDataStream
    * will handle the plumbing...
    */

  protected def initialFuture: Future[T]
  private [this] var cachedCurrentFuture = initialFuture

  /** May be called by the implementation to notify whoever's listening to the
    * stream that a future for a new value is available.
    */

  protected def noteNewFuture( fut: Future[T] ): Unit = {
    this.cachedCurrentFuture = fut
    for (listener <- listeners.values)
      fut.onSuccess{ listener( _ ) }
  }

  /** May be called by the implementation to notify that a particular new
    * value has arrived.
    */
  
  protected def noteNewValue( value: T ): Unit = {
    this.cachedCurrentFuture = Future( value )
    for (listener <- listeners.values)
      listener( value )
  }

  /** Receive this stream's next available value, and further values from this
    * stream as they become available, due changes in the underlying
    * resources.
    *
    * If caller is on an Android `HandlerThread`, values will be posted
    * back to the same thread.
    */

  def withValues( handler: T => Unit ): Unit =
    addListener( new Object, handler )  // dummy tag

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

  def addListener( tag: AnyRef, handler: T => Unit ): Unit = {
    cachedCurrentFuture.onSuccess{ handler( _ ) }
    listeners( tag ) = CallbackManager.wrapHandler( handler )
  }

  /** Remove the listener associated with the given tag */

  def removeListener( tag: AnyRef ): Unit = listeners.remove( tag )
    
  protected def hasListeners = !listeners.isEmpty

  private[this] var listeners = new HashMap[ AnyRef, T => Unit ]
}

/** Class representing a condition that starts or stops (e.g., some
  * activity being active.  In implementation, it is a DataStream of
  * element type DurationEvent, which in turn has the two distinguished
  * values DurationStart and DurationStop.
  *
  * It is conceded that the naming here sucks.
  */

trait Duration extends DataStream[DurationEvent]

class DurationEvent
case object DurationStart extends DurationEvent
case object DurationStop extends DurationEvent

/** Useful base class (or trait) for DataStreams whose implementations
  * naturally call only `noteNewValue`, and not `notifyNewValue`.
  */

trait ExplicitNotificationDataStream[T] extends DataStream[T]
{
  final protected override def noteNewFuture( fut: Future[T] ) =
    throw new RuntimeException( "Called noteNewFuture on a " + 
                                this.getClass.getName )

  protected def initialFuture = new Future[T]
  private var haveInitialValue = false

  override protected def noteNewValue( newVal: T ): Unit = {
    if (! haveInitialValue ) {
      haveInitialValue = true
      initialFuture.succeed( newVal )
    }
    super.noteNewValue( newVal )
  }
}

private class DurationFilteredDataStream[T]( duration: Duration,
                                             underlying: DataStream[T] )
  extends ExplicitNotificationDataStream[T]
{
  duration.withValues{  _ match {

    case DurationStart =>
      underlying.addListener( this, this.noteNewValue( _ ))

    case DurationStop =>
      underlying.removeListener( this )
  }}
}

private[notifications] 
class FlatMapProxyStream[T,V]( mapFunc: T => DataStream[V],
                               underlying: DataStream[T] ) 
  extends ExplicitNotificationDataStream[V]
{
  private[this] var vstream: DataStream[V] = null

  underlying.withValues{ newVal => {
    if (vstream != null) vstream.removeListener( this )
    vstream = mapFunc( newVal )
    vstream.addListener( this, this.noteNewValue( _ ))
  }}
}

private[notifications] 
class MapProxyStream[T,V]( mapFunc: T => V,
                           underlying: DataStream[T] ) 
  extends ExplicitNotificationDataStream[V]
{
  underlying.withValues{ value => this.noteNewValue( mapFunc( value )) }
}

private[notifications] 
class FilterProxyStream[T,V]( pred: T => Boolean,
                              underlying: DataStream[T] ) 
  extends ExplicitNotificationDataStream[T]
{
  underlying.withValues{ value =>
    if (pred( value ))
      this.noteNewValue( value )
  }
}

