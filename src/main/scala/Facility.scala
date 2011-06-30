package org.positronic.util

import _root_.android.util.Log
import _root_.android.content.Context

import _root_.java.util.concurrent.BlockingQueue
import _root_.java.util.concurrent.LinkedBlockingQueue

class AppFacility( logTag: String = null ) 
{
  var openNesting: Int = 0

  def getLogTag = logTag

  def realOpen(ctx: Context) = {}
  def realClose = {}

  def openInContext( ctx: Context ) = {
    if (openNesting == 0) {
      realOpen( ctx )
    }
    openNesting += 1
  }

  def close = {
    openNesting -= 1
    if (openNesting == 0) {
      realClose
    }
  }

  def log( s: String ) = {
    if (logTag != null) Log.d( logTag, s )
  }
}

trait WorkerThread extends AppFacility
{
  val q = new LinkedBlockingQueue[ () => Unit ]

  class QueueRunnerThread( q: BlockingQueue[() => Unit] ) extends Thread {
    var dying = false
    def prepareToExit = { dying = true }
    override def run = {
      while (!dying) {
        try {
          (q.take())()
        }
        catch {
          case ex: Throwable => Log.e( 
            if (getLogTag != null) getLogTag else "DB",
            "Uncaught exception on DB Thread",
            ex)
        }
      }
    }
  }

  def runOnThread( func: => Unit ) = {
    q.put( () => func )
  }

  var theThread: QueueRunnerThread = null

  override def realOpen( ctx: Context ) = {
    super.realOpen( ctx )
    theThread = new QueueRunnerThread( q )
    theThread.start
  }

  override def realClose = {

    // We want the thread to exit *after* draining all its current
    // work.  So, we put a "please go away" marker on the queue, and
    // wait for it to drain...

    runOnThread { theThread.prepareToExit }
    theThread.join
    theThread = null
    super.realClose
  }
  
  def assertOnThread( s: String ) {
    // Don't build the message unless we're going to throw the error...
    if ( Thread.currentThread != theThread ) {
      assert( Thread.currentThread == theThread, s + " called off DB Thread" )
    }
  }

}
