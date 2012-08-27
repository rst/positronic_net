package org.positronicnet.content

import _root_.android.content.Context
import _root_.android.content.ContentValues
import _root_.android.content.ContentUris
import _root_.android.content.ContentResolver
import _root_.android.net.Uri
import _root_.android.util.Log
import _root_.android.database.ContentObserver // android.database?  really?!!!

import org.positronicnet.facility.AppFacility
import org.positronicnet.facility.WorkerThread
import org.positronicnet.notifications.{ Future,
                                         DataStream, 
                                         StreamQuery, 
                                         StreamQueryable }

/** Simple [[org.positronicnet.facility.AppFacility]] for interacting with
  * Android ContentProviders using the Positronic Net
  * [[org.positronicnet.content.ContentQuery]] convenience shorthands,
  * or the [[org.positronicnet.orm]].  Once the facility has been
  * opened (via `openInContext`), it can be used to produce
  * [[org.positronicnet.content.ContentQuery]] objects that refer to
  * individual `ContentProvider`s, including those provided by other
  * apps or the platform itself.
  *
  * The best treatment of row-IDs here isn't quite clear.  The
  * underlying platform `insert` method on a `ContentProvider` returns
  * a URI for the newly created row, but by convention that row will
  * almost always include a `Long` which can be obtained from the URI
  * itself via the `ContentURIs.parseID` method provided by the
  * framework, and which will also be used to identify the row in the
  * same bulk queries.  Since it isn't obvious which is better, we
  * give you both choices, as follows:
  *
  * Let's say you have the URI of some
  * `ContentProvider` --- say `android.provider.CallLog.Calls.CONTENT_URI`,
  * just to be concrete.  Once you've got a resolver opened like so:
  * {{{
  *     object Resolver extends PositronicContentResolver( "call_log_app" )
  *     ...
  *     class SomeActivity {
  *       onCreate {
  *         useAppFacility( Resolver )
  *       }
  *     }
  * }}}
  * you can either say
  * {{{
  *     queryHandle = Resolver( Calls.CONTENT_URI )
  * }}}
  * to get a [[org.positronicnet.orm.ContentQuery]] on which `insert` will
  * return a `Long` ID, or
  * {{{
  *     queryHandle = Resolver.withUriIds( Calls.CONTENT_URI )
  * }}}
  * to obtain one on which `insert` is typed to return the raw URIs.
  *
  * There's no other difference between the two, but only the latter
  * will work with the [[org.positronicnet.orm]], which requires long
  * IDs for the moment.
  *
  * In general, the `Long`-id variants are easier to deal with for
  * providers which support that convention (most of the standard
  * ones on the platform, for starters), but the `withUriIds` variant
  * is there if you prefer it.
  *
  * There are also limited facilities for using
  * [[org.positronicnet.content.PositronicContentResolver]] for asynch
  * operations, using actor-like syntax; in particular, batch updates are
  * supported via [[org.positronicnet.content.BatchOperation]].
  *
  * (The underlying `android.content.ContentResolver` is available by
  * calling `realResolver`, q.v.; this, of course, supports the full,
  * unrestricted plafrom API.)
  */

object PositronicContentResolver
  extends AppFacility( "PositronicContentResolver" )
  with StreamQueryable[ ContentResolverQueryNonce ]
  with WorkerThread
{
  var logTag: String = "PositronicContentResolver"

  override def getLogTag = logTag

  private[positronicnet] var realResolver: android.content.ContentResolver = null

  override protected def realOpen(ctx: Context) = { 
    super.realOpen( ctx )
    realResolver = ctx.getContentResolver
  }

  /** Response to stream queries.  Right now, the sole supported query
    * argument is OnDataAvailable(uri), which creates a ContentObserver
    * that for the URI in question which sends a Unit when new data is
    * available (as well as a courtesy notification the first time a
    * listener subscribes, as per usual, to open the pipes.
    *
    * The usual convention is to map that Unit to something else, like
    * the result of a query, either direct, or through the ORM...
    */

  def ??[V]( q: StreamQuery[ ContentResolverQueryNonce, V ] ) = q match {
    case OnDataAvailable( uri, andSubtree ) =>
      new ContentObservationDataStream( uri, andSubtree )
  }

  /** Return a [[org.positronicnet.orm.ContentQuery]] obeying the `Long`-id
    * convention, as described above.
    */

  def apply( uri: Uri ) =
    new ContentProviderQuery( new LongIdContentResolverRepository(realResolver,
                                                                  this ),
                              uri )

  /** Return a [[org.positronicnet.orm.ContentQuery]] obeying the URI-id
    * convention, as described above.
    */

  def withUriIds( uri: Uri ) = 
    new ContentProviderQuery( new UriIdContentResolverRepository(realResolver,
                                                                 this),
                              uri )

  /** Run a content-resolver action on a private thread, invoking
    * callbacks on the caller's thread when done.
    */

  def !( action: ContentResolverAction ) = 
    action match {
      case batch: BatchAction => 
        val wrappedBatch = batch.withWrappedCallbacks
        this.runOnThread { this.onThisThread( wrappedBatch ) }
      case _ =>
        this.runOnThread { this.onThisThread( action ) }
    }

  /** Run a content-resolver action on the current thread */

  def onThisThread( action: ContentResolverAction ) =
    action match {
      case batch: BatchAction =>
        try { 
          Log.d( "PositronicContentResolver", "Running batch:" )

          for (i <- Range( 0, batch.operations.size ))
            Log.d( "PositronicContentResolver", 
                   batch.operations.get(i).toString )

          Log.d( "PositronicContentResolver", "End batch" )

          val results = realResolver.applyBatch( batch.authority,
                                                 batch.operations )
          batch.successCallback( results )
        }
        catch {
          case ex: Exception =>
            Log.e( "PositronicContentResolver", "Batch operation failure", ex )
            batch.failureCallback( ex )
        }
      case _ =>
        throw new RuntimeException( "Unknown ContentResolverAction" )
    }
}

/** Action on the content resolver, considered as an actor */

abstract class ContentResolverAction

private [content]
abstract class BaseContentResolverRepo[ IdType ]( realResolver: ContentResolver,
                                                  facilityArg: AppFacility )
  extends ContentRepository[ Uri, IdType ]
{
  def facility  = facilityArg
  def getLogTag = facility.getLogTag

  def delete( whence: Uri, where: String, whereArgs: Array[String] ) = 
    realResolver.delete( whence, where, whereArgs )

  def update( whence: Uri, vals: ContentValues, 
              where: String, whereArgs: Array[ String ] ) =
    realResolver.update( whence, vals, where, whereArgs )

  // Note that we ignore limit, groupBy, and order; ContentProviderQuery
  // gives users no way to set them, so they're NULL unless someone's
  // playing very nasty games on us...

  def query( whence: Uri, cols: Array[ String ], 
             where: String, whereArgs: Array[ String ],
             groupBy: String, having: String,
             order: String, limit: String ) =
    realResolver.query( whence, cols, where, whereArgs, order )
}

class UriIdContentResolverRepository( realResolver: ContentResolver,
                                      facility: AppFacility )
  extends BaseContentResolverRepo[ Uri ]( realResolver, facility )
{
  def insert( where: Uri, vals: ContentValues ) =
    realResolver.insert( where, vals )
}

class LongIdContentResolverRepository( realResolver: ContentResolver,
                                       facility: AppFacility )
  extends BaseContentResolverRepo[ Long ]( realResolver, facility )
{
  def insert( where: Uri, vals: ContentValues ) =
    ContentUris.parseId( realResolver.insert( where, vals ))
}

/** Queries on ContentProviders. See
  * [[org.positronicnet.content.PositronicContentResolver]]
  */

class ContentProviderQuery[IdType]( source: BaseContentResolverRepo[IdType],
                                    uri: Uri,
                                    orderString: String = null,
                                    whereString: String = null,
                                    whereValues: Array[String] = null
                                  ) 
  extends ContentQuery( source, uri, orderString,
                        whereString, whereValues, 
                        limitString = null )
{
  protected def dinkedCopy( source: BaseContentResolverRepo[IdType]=this.source,
                            uri: android.net.Uri         = this.uri,
                            orderString: String          = this.orderString,
                            whereString: String          = this.whereString,
                            whereValues: Array[String]   = this.whereValues ) =
    new ContentProviderQuery( source, uri, orderString, 
                              whereString, whereValues )

  def contentUri = uri

  def order( s: String ) = dinkedCopy( orderString = s )

  def where( s: String, vals: ContentValue* ) =
    withUpdatedWhere( s, vals.toArray ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  def whereEq( pairs: (String, ContentValue)* ) =
    withUpdatedWhere( pairs ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  def facility = source.facility

  def count:Long =
    throw new RuntimeException( "Count not supported on ContentResolvers" )

  def limit( s: String ) =
    throw new RuntimeException( "Limit not supported on ContentResolvers" )
}

class ContentResolverStreamQuery[V] 
  extends StreamQuery[ContentResolverQueryNonce, V]

class ContentResolverQueryNonce

case class OnDataAvailable( uri: Uri, andSubtree: Boolean = false ) 
  extends ContentResolverStreamQuery[ Unit ]

private[ positronicnet ]
class ContentObservationDataStream( uri: Uri, andSubtree: Boolean )
  extends DataStream[Unit]
{
  protected def initialFuture = Future( () )

  override def addListener( tag: AnyRef, handler: Unit => Unit ) = {
    super.addListener( tag, handler )
    maybeStartObserving
  }

  override def removeListener( tag: AnyRef ) = {
    super.removeListener( tag )
    if (!this.hasListeners)
      maybeStopObserving
  }

  private [this] var observer: ContentObserver = null
  private [this] var observing = false

  import PositronicContentResolver.realResolver

  private [this] def maybeStartObserving = {
    if (!observing) {
      PositronicContentResolver.threadHandlerFut.map { handler => {
        observer = new ContentObserver( handler ) {
          override def onChange( dummy: Boolean ) = noteNewValue( () )
        }
        realResolver.registerContentObserver( uri, andSubtree, observer )
      }}
    }
  }

  private [this] def maybeStopObserving = {
    if (observing) {
      observing = false
      realResolver.unregisterContentObserver( observer )
    }
  }
}
