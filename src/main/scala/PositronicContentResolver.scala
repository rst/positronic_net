package org.positronicnet.content

import _root_.android.content.Context
import _root_.android.content.ContentValues
import _root_.android.content.ContentUris
import _root_.android.net.Uri

import org.positronicnet.util.AppFacility
import org.positronicnet.util.WorkerThread

// Basic machinery for wrapping ContentResolver and friends.

class PositronicContentResolver( logTag: String = null ) 
  extends AppFacility( logTag )
  with ContentSource[ Uri ]
{
  var realResolver: android.content.ContentResolver = null

  override def realOpen(ctx: Context) = { 
    realResolver = ctx.getContentResolver
  }

  def apply( uri: Uri ) = new ContentProviderQuery( this, uri )

  def delete( whence: Uri, where: String, whereArgs: Array[String] ) = 
    realResolver.delete( whence, where, whereArgs )

  def update( whence: Uri, vals: ContentValues, 
              where: String, whereArgs: Array[ String ] ) =
    realResolver.update( whence, vals, where, whereArgs )

  def insert( where: Uri, vals: ContentValues ) =
    ContentUris.parseId( realResolver.insert( where, vals ))

  // Note that we ignore limit, groupBy, and order; ContentProviderQuery
  // gives users no way to set them, so they're NULL unless someone's
  // playing very nasty games on us...

  def query( whence: Uri, cols: Array[ String ], 
             where: String, whereArgs: Array[ String ],
             groupBy: String, having: String,
             order: String, limit: String ) =
    realResolver.query( whence, cols, where, whereArgs, order )
}

// Queries on ContentResolvers.

class ContentProviderQuery( source: ContentSource[ android.net.Uri ], 
                            uri: Uri,
                            orderString: String = null,
                            whereString: String = null,
                            whereValues: Array[String] = null
                          ) 
  extends ContentQuery[ android.net.Uri ]( source, uri, orderString,
                                           whereString, whereValues, 
                                           limitString = null )
{
  protected def dinkedCopy( source: ContentSource[ Uri ] = this.source, 
                            uri: android.net.Uri         = this.uri,
                            orderString: String          = this.orderString,
                            whereString: String          = this.whereString,
                            whereValues: Array[String]   = this.whereValues ) =
    new ContentProviderQuery( source, uri, orderString, 
                              whereString, whereValues )

  def order( s: String ) = dinkedCopy( orderString = s )

  def where( s: String, arr: Array[ContentValue] = null ) =
    withUpdatedWhere( s, arr ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  def whereEq( pairs: (String, ContentValue)* ) =
    withUpdatedWhere( pairs ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}
}



