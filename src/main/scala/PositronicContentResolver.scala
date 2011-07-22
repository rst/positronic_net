package org.positronicnet.content

import _root_.android.content.Context
import _root_.android.content.ContentValues
import _root_.android.content.ContentUris

import org.positronicnet.util.AppFacility
import org.positronicnet.util.WorkerThread

// Class for database singletons.  Manages a SQLiteOpenHelper (details
// below), with some mutual delegation.

class PositronicContentResolver( logTag: String = null ) 
  extends AppFacility( logTag )
  with ContentSource
{
  var realResolver: android.content.ContentResolver = null

  override def realOpen(ctx: Context) = { 
    realResolver = ctx.getContentResolver
  }

  def apply( uri: String ) = new ContentProviderQuery( this, uri )

  def delete( whence: String, where: String, whereArgs: Array[String] ) = 
    realResolver.delete( asUri( whence ), where, whereArgs )

  def update( whence: String, vals: ContentValues, 
              where: String, whereArgs: Array[ String ] ) =
    realResolver.update( asUri( whence ), vals, where, whereArgs )

  def insert( where: String, vals: ContentValues ) =
    ContentUris.parseId( realResolver.insert( asUri( where ), vals ))

  // Note that we ignore limit, groupBy, and order; ContentProviderQuery
  // gives users no way to set them, so they're NULL unless someone's
  // playing very nasty games on us...

  def query( whence: String, cols: Array[ String ], 
             where: String, whereArgs: Array[ String ],
             groupBy: String, having: String,
             order: String, limit: String ) =
    realResolver.query( asUri( whence ), cols, where, whereArgs, order )

  private def asUri( s: String ) = android.net.Uri.parse( s )
}

// Queries on Databases.

class ContentProviderQuery( source: ContentSource, 
                            tableName: String,
                            orderString: String = null,
                            whereString: String = null,
                            whereValues: Array[String] = null
             ) 
  extends ContentQuery( source, tableName, orderString,
                        whereString, whereValues, limitString = null )
{
  protected def dinkedCopy( source: ContentSource      = this.source, 
                            tableName: String          = this.tableName,
                            orderString: String        = this.orderString,
                            whereString: String        = this.whereString,
                            whereValues: Array[String] = this.whereValues ) =
    new ContentProviderQuery( source, tableName, orderString, 
                              whereString, whereValues )

  def order( s: String ) = dinkedCopy( orderString = s )

  def where( s: String, arr: Array[ContentValue] = null ) =
    withUpdatedWhere( s, arr ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  def whereEq( pairs: (String, ContentValue)* ) =
    withUpdatedWhere( pairs ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}
}



