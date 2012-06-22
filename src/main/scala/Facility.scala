package org.positronicnet.facility

import _root_.android.util.Log
import _root_.android.content.Context
import _root_.android.os.{Handler, HandlerThread, Message}

import _root_.java.util.concurrent.BlockingQueue
import _root_.java.util.concurrent.LinkedBlockingQueue

import org.positronicnet.notifications.Future

/** Semi-abstract base class for a facility of some kind which is
  * of use to one or more Activities or Services within an application.
  * Often has a [[org.positronicnet.facility.WorkerThread]], which it
  * can use to run code in the background.
  */

class AppFacility( logTag: String = null ) 
{
  private var openNesting: Int = 0

  /** Tag for the facility to use in log entries */

  def getLogTag = logTag

  protected def realOpen(ctx: Context) = {}
  protected def realClose = {}

  /** Open the facility.
    *
    * If the facility isn't already open, does setup which depends on
    * the particular facility (which may involve opening files,
    * starting threads, or whatever) --- a "real" open, using the
    * protected `realOpen` method.  The given `Context` may be used
    * for setup, depending on the nature of the facility.
    *
    * If it is already open, just increments a "nested open" count;
    * see `close` below.
    */

  def openInContext( ctx: Context ) = {
    if (openNesting == 0) {
      realOpen( ctx )
    }
    openNesting += 1
  }

  /** Close the facility.
    *
    * First, decrements the "nested open" count.  If it has reached
    * zero, do a "real" close, using the protected `realclose` method.
    * Otherwise, some other activity is still using the facility, and
    * it stays open.
    *
    * When the last activity or service calls `close`, the facility
    * actually does shut down, using the protected `realClose` method,
    * which will shut down threads, close files, or whatever.
    */

  def close = {
    openNesting -= 1
    if (openNesting == 0) {
      realClose
    }
  }

  /** Writes a debugging log method, using the facility's `logTag` */

  def log( s: String ) = {
    if (logTag != null) Log.d( logTag, s )
  }
}

/** Mixin for [[org.positronicnet.facility.AppFacility]] which provides
  * a "worker thread", which may be used to run things in the background.
  */

trait WorkerThread extends AppFacility
{
  private [this] val openCloseLock = new Object

  private [positronicnet]
  var threadHandlerFut = new Future[Handler]

  private
  class QueueRunnerThread
    extends HandlerThread( this.toString ) 
  {
    threadHandlerFut.succeed( new Handler() )
  }

  private [this]
  var theThread: QueueRunnerThread = null

  /** Run `func` on the facility's thread.
    */

  def runOnThread( thunk: => Unit ): Unit =
    threadHandlerFut.onSuccess{ _.post( new Runnable { def run = thunk })}

  override protected def realOpen( ctx: Context ) = {
    openCloseLock.synchronized {
      super.realOpen( ctx )
      theThread = new QueueRunnerThread
      theThread.start
    }
  }

  override protected def realClose = {
    openCloseLock.synchronized {
      theThread.quit
      theThread.join
      theThread = null
      threadHandlerFut = new Future[Handler] // for reopen
      super.realClose
    }
  }
  
  /** Throws an assertion error if called from anywhere but this
    * activity's thread.
    */

  def assertOnThread( s: String ) {
    // Don't build the message unless we're going to throw the error...
    if ( Thread.currentThread != theThread ) {
      assert( Thread.currentThread == theThread, s + " called off facility Thread" )
    }
  }

}
