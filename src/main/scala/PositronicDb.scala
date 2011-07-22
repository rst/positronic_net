package org.positronicnet.db

import org.positronicnet.content._

import org.positronicnet.util.AppFacility
import org.positronicnet.util.WorkerThread

import _root_.android.database.sqlite._
import _root_.android.content.Context
import _root_.android.content.ContentValues

// Class for database singletons.  Manages a SQLiteOpenHelper (details
// below), with some mutual delegation.

abstract class Database( filename: String, logTag: String = null ) 
  extends AppFacility( logTag )
{
  var dbWrapper: DbWrapper = null

  def getFilename = filename

  override def realOpen(ctx: Context) = { 
    dbWrapper = new DbWrapper( ctx, this ) 
  }

  override def realClose = { 
    dbWrapper.close; dbWrapper = null 
  }

  // List of "one-way migrations" that define the current DB schema...

  def schemaUpdates: List[String]

  // Versions of the common parts of the SQLiteOpenHelper API.
  // The real SQLiteOpenHelper that we create in realOpen
  // delegates to these...

  def getWritableDatabase = dbWrapper.getWritableDatabase
  def getReadableDatabase = dbWrapper.getReadableDatabase
  
  def onCreate( db: SQLiteDatabase ) = onUpgrade( db, 0, version )
  
  def onUpgrade( db: SQLiteDatabase, oldVersion: Int, newVersion: Int ) = {
    assert( version == newVersion )
    // XXX --- doesn't work for update/insert/delete; need to think
    // about data migrations.  Doing a regex match to see if the
    // first nonblank word is "insert", "update", etc. ought to do it.
    for ( sql <- schemaUpdates.drop( oldVersion )) {
      log( "Running schema upgrade: " + sql )
      db.execSQL( sql ) 
    }
  }

  def version = schemaUpdates.length

  def apply( table: String ) = new DbQuery( dbWrapper, table )
}

// Database interface.

class DbWrapper( ctx: Context, mydb: Database ) 
  extends SQLiteOpenHelper( ctx, mydb.getFilename, null, mydb.version )
  with ContentSource
{
  def onCreate( db: SQLiteDatabase ) = mydb.onCreate( db )
  
  def onUpgrade( db: SQLiteDatabase, oldVersion: Int, newVersion: Int ) = 
    mydb.onUpgrade( db, oldVersion, newVersion )

  def getLogTag = mydb.getLogTag

  def delete( whence: String, where: String, whereArgs: Array[String] ) = 
    getWritableDatabase.delete( whence, where, whereArgs )

  def update( whence: String, vals: ContentValues, 
              where: String, whereArgs: Array[ String ] ) =
    getWritableDatabase.update( whence, vals, where, whereArgs )

  def insert( where: String, vals: ContentValues ) =
    getWritableDatabase.insert( where, null, vals )

  def query( whence: String, cols: Array[ String ], 
             where: String, whereArgs: Array[ String ],
             groupBy: String, having: String,
             order: String, limit: String ) =
    getWritableDatabase.query( whence, cols, 
                               where, whereArgs, 
                               groupBy, having,
                               order, limit )
}

// Queries on Databases.

class DbQuery( source: ContentSource, 
               tableName: String,
               orderString: String = null,
               whereString: String = null,
               whereValues: Array[String] = null,
               limitString: String = null
             ) 
  extends ContentQuery( source, tableName, orderString,
                        whereString, whereValues, limitString )
{
  protected def dinkedCopy( source: ContentSource      = this.source, 
                            tableName: String          = this.tableName,
                            orderString: String        = this.orderString,
                            whereString: String        = this.whereString,
                            whereValues: Array[String] = this.whereValues,
                            limitString: String        = this.limitString ) =
    new DbQuery( source, tableName, orderString, 
                 whereString, whereValues, limitString )

  def order( s: String ) = { dinkedCopy( orderString = s ) }
  def limit( s: String ) = { dinkedCopy( limitString = s ) }
  def limit( l: Int )    = { dinkedCopy( limitString = l.toString ) }

  def where( s: String, arr: Array[ContentValue] = null ):DbQuery =
    withUpdatedWhere( s, arr ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  def whereEq( pairs: (String, ContentValue)* ):DbQuery =
    withUpdatedWhere( pairs ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  // Non-generic query variants...

  def oneRow( cols: String* ) = limit( 1 ).select( cols:_* )

  def count:Long = {
    val c = this.select( "count(*)" );
    c.moveToFirst
    val result = c.getLong(0)
    c.close
    return result
  }
}



