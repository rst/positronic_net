package org.positronicnet.content

import _root_.android.content.Context
import _root_.android.content.ContentValues
import _root_.android.content.ContentUris
import _root_.android.content.ContentResolver
import _root_.android.net.Uri

import org.positronicnet.facility.AppFacility
import org.positronicnet.facility.WorkerThread

// Basic machinery for wrapping ContentResolver and friends.
// SourceType is android.net.Uri.  IdType can be either Long
// or URI, depending on what the caller wants; it must be
// Long to use with the ORM.

class PositronicContentResolver( logTag: String = null ) 
  extends AppFacility( logTag )
{
  var realResolver: android.content.ContentResolver = null

  override def realOpen(ctx: Context) = { 
    realResolver = ctx.getContentResolver
  }

  def apply( uri: Uri ) = 
    new ContentProviderQuery( new LongIdContentResolverRepository(realResolver,
                                                                  this ),
                              uri )

  def withUriIds( uri: Uri ) = 
    new ContentProviderQuery( new UriIdContentResolverRepository(realResolver,
                                                                 this),
                              uri )
}

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

// Queries on ContentResolvers.

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



