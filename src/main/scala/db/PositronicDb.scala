package org.positronicnet.db

import org.positronicnet.content._
import org.positronicnet.facility._

import _root_.android.database.sqlite._
import _root_.android.content.Context
import _root_.android.content.ContentValues

/** Class for database singletons.  Manages setup and schema upgrades,
  * and makes the tables database available as Positronic Net
  * [[org.positronicnet.content.ContentRepository]]s which can be
  * queried directly, or manipulated via the [[org.positronicnet.orm]].
  *
  * Or, if you just want the schema maintenance support, you can call
  * `getReadableDatabase` and `getWritableDatabase`, and use the standard
  * platform APIs for everything else.
  *
  * Most behavior is inherited from [[org.positronicnet.db.ThreadlessDatabase]].
  * (In fact, this is just [[org.positronicnet.db.ThreadlessDatabase]] with
  * [[org.positronicnet.facility.WorkerThread]] mixed in.  On Android, it's
  * usually best practice to run database work in the background, so by
  * default, the machinery to facilitate that is there; if you really want
  * a [[org.positronicnet.db.ThreadlessDatabase]], you can get one, but you
  * do have to ask.)
  *
  * ==Schema definition and management==
  *
  * Note that `open`ing a [[org.positronicnet.db.Database]] does ''not''
  * trigger any I/O; that action is delayed until the first query (at
  * which point we also perform any schema updates, etc.).  It is strongly
  * recommended that this ''not'' be done on an `Activity` main UI thread.
  *
  * Schema definitions are ordinarily done by supplying a list of table
  * definitions, `ALTER TABLE` upgrades, etc., as the value of `schemaUpdates`.
  * The idea is that each of these statements defines a database upgrade.
  * The way this relates to the standard `SQLiteOpenHelper` machinery is as
  * follows:
  *
  *  - `version` is the number of statements on the list
  *  - `onCreate` runs them all
  *  - `onUpdate` runs all after the old version, to get to the current
  *    version
  *
  * The upshot is that you can add statements to the list; the current
  * schema is defined by what you get from evaluating them all, and
  * updates will run the missing ones on the fly.
  *
  * (This arrangement is effectively a very cut-down version of the
  * migrations system in the Ruby on Rails ActiveRecord ORM, which
  * also features database versions defined implicitly by a stream of
  * updates.  However, there's no support here for schema ''down''grades,
  * nor for transparently handling updates coming from multiple
  * development streams.)
  *
  * ==Using the database==
  *
  * There are two ways to use a database, once you've opened it.
  *
  * The first is simply to call `getWritableDatabase` and `getReadableDatabase`,
  * which gets you a straight `SQLiteDatabase` object, with the full standard
  * API.
  *
  * The second is that if you have a database named, say, `TodoDb`, with
  * a table named `todo_items` then `TodoDb("todo_items")` returns a
  * [[org.positronicnet.content.ContentQuery]] which may be used to manipulate
  * all rows in the table directly, or through a
  * [[org.positronicnet.orm.RecordManager]].
  */

abstract class Database( filename: String, logTag: String = null )
  extends ThreadlessDatabase( filename, logTag )
  with WorkerThread

/** Primitive base for database singletons.  For most purposes, you'll
  * want to use [[org.positronicnet.db.Database]] instead, for its extra
  * convenience features.  [[org.positronicnet.db.ThreadlessDatabase]] is
  * available for cases where the conveniences are getting in the way.
  *
  * Manages setup and schema upgrades,
  * and makes the tables database available as Positronic Net
  * [[org.positronicnet.content.ContentRepository]]s which can be
  * queried directly, or manipulated via the [[org.positronicnet.orm]].
  * Further comments on how this works for [[org.positronicnet.db.Database]]
  * also apply here.
  *
  * Note that `open`ing a [[org.positronicnet.db.Database]] does ''not''
  * trigger any I/O; that action is delayed until the first query (at
  * which point we also perform any schema updates, etc.).  It is strongly
  * recommended that this ''not'' be done on an `Activity` main UI thread.
  */
abstract class ThreadlessDatabase( filename: String, logTag: String = null ) 
  extends AppFacility( logTag )
{
  private [db] var dbWrapper: DbWrapper = null

  private [db] def getFilename = filename

  override protected def realOpen(ctx: Context) = { 
    dbWrapper = new DbWrapper( ctx, this ) 
  }

  override protected def realClose = { 
    dbWrapper.close; dbWrapper = null 
  }

  /** List of "one-way migrations" that define the current DB schema...
    * See the [[org.positronicnet.db.Database]] overview.
    */

  def schemaUpdates: List[String]

  /** Get a writable Android `SQLiteDatabase` object, for direct invocation
    * of core APIs.
    */

  def getWritableDatabase = dbWrapper.getWritableDatabase

  /** Get a readable Android `SQLiteDatabase` object, for direct invocation
    * of core APIs.
    */

  def getReadableDatabase = dbWrapper.getReadableDatabase
  
  /** Invoked when the database is created.  Default behavior is to
    * run through the complete list of `schemaUpdates`.
    */

  def onCreate( db: SQLiteDatabase ) = onUpgrade( db, 0, version )
  
  /** Invoked when the database is created.  Default behavior is to
    * run through the sublist of `schemaUpdates` from the prior version
    * to the end (which defines the current version).
    */

  def onUpgrade( db: SQLiteDatabase, oldVersion: Int, newVersion: Int ) = {
    assert( version == newVersion )
    for ( sql <- schemaUpdates.drop( oldVersion )) {
      log( "Running schema upgrade: " + sql )
      db.execSQL( sql ) 
    }
  }

  /** Schema version.  By default, the length of the `schemaUpdates` list. */

  def version = schemaUpdates.length

  /** Returns a [[org.positronicnet.content.ContentQuery]] referring
    * to all rows in one of the tables of this database, which may be
    * used directly, or used to seed a [[org.positronicnet.orm.RecordManager]]
    * for the ORM.
    */

  def apply( table: String ) = new DbQuery( dbWrapper, table )
}

// Database interface.
//
// Functions as a ContentRepository; SourceType is String (table name),
// IdType is Long (row id).

private [db]
class DbWrapper( ctx: Context, mydb: ThreadlessDatabase ) 
  extends SQLiteOpenHelper( ctx, mydb.getFilename, null, mydb.version )
  with ContentRepository[ String, Long ]
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

  def facility = mydb
}

/** Queries on Databases.  Supports the [[org.positronicnet.orm.ContentQuery]]
  * API in full, including `limit` and `order`.
  */

class DbQuery( source: DbWrapper, 
               tableName: String,
               orderString: String = null,
               whereString: String = null,
               whereValues: Array[String] = null,
               limitString: String = null
             ) 
  extends ContentQuery( source, tableName, orderString,
                        whereString, whereValues, limitString )
{
  private def dinkedCopy( source: DbWrapper           = this.source, 
                          tableName: String           = this.tableName,
                          orderString: String         = this.orderString,
                          whereString: String         = this.whereString,
                          whereValues: Array[String]  = this.whereValues,
                          limitString: String         = this.limitString ) =
    new DbQuery( source, tableName, orderString, 
                 whereString, whereValues, limitString )

  def order( s: String ) = dinkedCopy( orderString = s )
  def limit( s: String ) = dinkedCopy( limitString = s )

  def where( s: String, vals: ContentValue* ):DbQuery =
    withUpdatedWhere( s, vals.toArray ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  def whereEq( pairs: (String, ContentValue)* ):DbQuery =
    withUpdatedWhere( pairs ){ (str, arr) => 
      dinkedCopy( whereString = str, whereValues = arr )}

  // Non-generic query variants...

  def oneRow( cols: String* ) = limit( 1 ).select( cols:_* )

  override def count:Long = {
    val c = this.select( "count(*)" );
    c.moveToFirst
    val result = c.getLong(0)
    c.close
    return result
  }

  def facility = source.facility
}



