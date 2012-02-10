package org.positronicnet.content

import android.content.{ContentProviderOperation, 
                        ContentProviderResult}
import java.util.ArrayList

import org.positronicnet.notifications.CallbackManager

/** Action on ContentResolvers that applies a batch of
  * `android.content.ContentProviderOperation`s.  See also
  * [[org.positronicnet.orm.BatchScopeAction]], which allows
  * you to run a batch of, e.g., `Save` and `Delete` ORM actions
  * on content mapped by the ORM to some content provider.
  */

class BatchAction private ( 
    val authority: String,
    private [positronicnet] val operations: ArrayList[ContentProviderOperation],
    private [positronicnet] val successCallback: (Array[ContentProviderResult]
                                                  => Unit),
    private [positronicnet] val failureCallback: (Exception => Unit))
  extends ContentResolverAction
{
  def this( authority: String ) = 
    this( authority,
          new ArrayList[ ContentProviderOperation ],
          (results => ()),
          (exception => ()))

  private def copy( 
    authority: String = this.authority,
    operations: ArrayList[ContentProviderOperation] = this.operations,
    successCallback: (Array[ContentProviderResult]=>Unit) =this.successCallback,
    failureCallback: (Exception => Unit) = this.failureCallback)
  = new BatchAction( authority, operations, successCallback, failureCallback)

  /** Fluid interface for adding a success callback to this batch
    * operation.  
    */

  def onSuccess( thunk: => Unit ) =
    this.copy( successCallback = ( results => thunk ))

  /** Fluid interface for adding a failure callback to this batch
    * operation.  The thrown exception is logged.
    */

  def onFailure( thunk: => Unit ) =
    this.copy( failureCallback = ( results => thunk ))

  /** Fluid interface for adding a success callback to this batch
    * operation.  The argument is the returned array of
    * `ContentProviderResult`s.
    */

  def withSuccessfulResults( callback: Array[ContentProviderResult] => Unit ) =
    this.copy( successCallback = callback )

  /** Fluid interface for adding a failure callback to this batch
    * operation.  The argument is the exception that was thrown;
    * this will typically be either an OperationApplicationException 
    * or a RemoteException.
    *
    * The thrown exception will have already been logged whatever else
    * the callback decides to do with it.
    */

  def withThrownException( callback: Exception => Unit ) =
    this.copy( failureCallback = callback )

  /** Add an operation to this batch */

  def add( op: ContentProviderOperation ) = operations.add( op )

  private [positronicnet]
  def withWrappedCallbacks =
    this.copy( 
      successCallback = CallbackManager.wrapHandler( successCallback ),
      failureCallback = CallbackManager.wrapHandler( failureCallback ))
}
                        
