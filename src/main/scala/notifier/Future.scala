package org.positronicnet.notifications

import scala.collection.mutable.ArrayBuffer
import android.util.Log

/** A cut-down version of the Futures from the Scala 2.10 standard library
  * (mostly), implementing the basics of both the Future and Promise sides
  * of the interface.
  *
  * A future represents the result of some asynchronous computation which
  * one thread is computing, which is of interest to at least one other.
  * Threads with an interest in the result can ask to be informed when it
  * is ready (via the onSuccess, onFailure, and onComplete methods), which
  * take a callback as an argument, and invoke it on the calling thread
  * when the result is available (possibly immediately).  For this to work,
  * the calling thread must be an Android `HandlerThread` (which includes
  * the threads that the framework itself invokes to run `Service`s and
  * `Activity` user interfaces).  If the result is already available, these
  * will complete immediately.
  *
  * The thread that is actually doing the work can call `succeed` or `fail`
  * to make a result available to other threads (or actor-like entities)
  * with an interest.
  *
  * The monadic future combinators from the Scala 2.10 and Akka libraries
  * are not yet available, but should probably show up in due course.
  */

class Future[T] {

  type Result = Either[ T, Throwable ] // Like Try[T] in the Scala 2.10 lib

  private[this] var result: Result = null
  private[this] val callbacks = new ArrayBuffer[ Result => Unit ]

  /** When this future has completed and succeeded, call the callback.
    *
    * If it already has, that'll be immediate.  Otherwise, caller must
    * be running on an Android `HandlerThread`, and the result will be
    * posted back to the same.
    */

  def onSuccess[V]( callback: T => V ) =
    onComplete{ case Left(t) => callback( t ) }

  /** When this future has completed and failed, call the callback.
    *
    * If it already has, that'll be immediate.  Otherwise, caller must
    * be running on an Android `HandlerThread`, and the result will be
    * posted back to the same.
    */

  def onFailure[V]( callback: Throwable => V ) =
    onComplete{ case Right(t) => callback( t ) }

  /** When this future has completed and succeeded, call the callback.
    *
    * If it already has, that'll be immediate.  Otherwise, caller must
    * be running on an Android `HandlerThread`, and the result will be
    * posted back to the same.
    */

  def onSuccess[V]( callback: PartialFunction[T, V] ) =
    onComplete{ case Left(t) => 
      if (callback.isDefinedAt( t ))
        callback(t) 
    }

  /** When this future has completed and failed, call the callback.
    *
    * If it already has, that'll be immediate.  Otherwise, caller must
    * be running on an Android `HandlerThread`, and the result will be
    * posted back to the same.
    */

  def onFailure[V]( callback: PartialFunction[Throwable, V] ) =
    onComplete{ case Right(t) =>
      if (callback.isDefinedAt( t ))
        callback( t ) 
    }

  /** When this future has completed, call the callback.
    *
    * If it already has, that'll be immediate.  Otherwise, caller must
    * be running on an Android `HandlerThread`, and the result will be
    * posted back to the same.
    */

  def onComplete[V]( callback: PartialFunction[ Result, V ] ) =
    if (result != null) {
      // Already had a value.  Go to work immediately.
      if (callback.isDefinedAt( result ))
        callback( result )
    }
    else {
      synchronized {
        // Check to see if we got a result since the first check,
        // to guard against race conditions.
        if (result != null) {
          if (callback.isDefinedAt( result ))
            callback( result )
        }
        else
          // Nope.  Bank callback for later.
          callbacks += CallbackManager.wrapPartialHandler( callback )
      }
    }

  /** We've succeeded.  Notify all callbacks, and any thread that blocked. */

  def succeed( value: T ) = complete( Left( value ))

  /** We've failed.  Notify all callbacks, and any thread that blocked. */

  def fail( exception: Throwable ) = {
    Log.e( "XXX", "Exception completing future", exception )
    complete( Right( exception ))
  }

  /** We've finished.  Notify all callbacks, and any thread that blocked. */

  def complete( result: Result ) = {
    synchronized {
      // set "have result" state, so we'll stop banking up callbacks.
      this.result = result
      this.notify
    }
    // Then drain the queue of the ones we've banked up.
    for (callback <- callbacks)
      callback( this.result )
  }

  /** Chaining:  complete this future with the result of some other
    * similarly typed future
    */

  def completeWith( other: Future[T] ) =
    other.onComplete {
      case Left( value ) => this.succeed( value )
      case Right( exc )  => this.fail( exc )
    }

  /** Applied to a future of v, this gives a future of f(v) */

  def map[V]( f: T => V ): Future[V] = {
    val vfuture = new Future[V]
    this.onComplete {
      case Right( exception ) => vfuture.complete( Right( exception ))
      case Left( value ) => 
        try {
          vfuture.complete( Left( f(value) ))
        }
        catch {
          case exception: Throwable => 
            vfuture.fail( exception )
        }
    }
    vfuture
  }

  /** Applied to a future of v, this gives a future equivalent to f(v) */

  def flatMap[V]( f: T => Future[V] ) = {
    val vfuture = new Future[V]
    this.onComplete {
      case Right( exception ) => vfuture.complete( Right( exception ))
      case Left( value )      => vfuture.completeWith( f( value ))
    }
    vfuture
  }

  /** Block until completion */

  def block = 
    synchronized { 
      if (this.result == null)
        this.wait 
    }
}

object Future {

  /** Construct a future from an ordinary value.
    *
    * Useful if you have code that's expecting a future, but you already
    * have the value now.
    */

  def apply[T]( obj: T ) = {
    val f = new Future[T]
    f.succeed( obj )
    f
  }

  import scala.collection.mutable.Builder
  import scala.collection.generic.CanBuildFrom

  /** Transform a collection of futures to a future of a collection.
    * For example, transforms `Seq(Future(3),Future(4))` to
    * `Future(Seq(3,4))`.
    */

  def sequence[A, M[_] <: Traversable[_]](in: M[Future[A]])
      (implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]]): Future[M[A]] = 
  {
    val builderFuture =
      in.foldLeft( Future(cbf(in)) )((fr, fa) => 
        for (r <- fr; 
             a <- fa.asInstanceOf[ Future[A] ]) 
        yield (r += a))
    
    builderFuture.map{ _.result }
  }
}
