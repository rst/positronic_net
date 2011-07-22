package org.positronicnet.db

import _root_.android.database.Cursor
import _root_.android.os.Bundle

import _root_.android.database.sqlite._
import _root_.android.database.ContentObserver
import _root_.android.database.DataSetObserver
import _root_.android.database.CharArrayBuffer

import _root_.android.content.ContentValues
import _root_.android.content.ContentResolver
import _root_.android.content.Context

import _root_.android.util.Log

import org.positronicnet.util.AppFacility
import org.positronicnet.util.WorkerThread
import org.positronicnet.util.ChangeNotifications

import scala.collection.mutable.ArrayBuffer

// Mummery to make sure that on inserts and updates, strings and ints
// are added into contentValues objects with the appropriate types.
// We declare an "ContentValue" variant type, declare these things as
// taking ((String, ContentValue)*) arguments, and supply implicit
// conversions from the primitive types to ContentValue.
//
// The only point of this, right at the moment, is to handle the
// Boolean conversions --- SQLite has no native boolean type, and
// the convention is integer columns valued 0 and 1.  If we had just
// Strings and Ints to deal with, we wouldn't need any of this crud,
// as SQLite converts at need internally.  (And many, many apps
// using the native Android API are already relying on that, 
// since 'where' clause values if supplied are bound as strings,
// but frequently used for comparison to int-valued columns.)

abstract class ContentValue {

  // Android's Database API has two forms of binding:  in a lot
  // of places, you can stuff things into a contentValues, which
  // holds typed objects --- but for bound variables in where
  // clauses (the various selectionArgs arguments), you can supply
  // only strings.  Fortunately, it matters less than one might
  // think, since the SQLite engine does conversions if you ask
  // it to compare a number to a string.  But it does mean that
  // if you're implementing conversions of your own --- as we
  // do with Booleans here, to implement the standard SQLite
  // convention of representing them as 0 or 1 --- you have to
  // specify the conversions both ways to keep it consistent.

  def asConditionString: String
  def putContentValues( cv: ContentValues, key: String )
}
case class CvString( value: String ) extends ContentValue {
  def asConditionString = value
  def putContentValues( cv:ContentValues, key:String ) = cv.put( key, value )
}
case class CvBoolean( value: Boolean ) extends ContentValue {
  def intValue = if (value) 1 else 0
  def asConditionString = intValue.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put(key, new java.lang.Integer(intValue))
}
case class CvInt( value: Int ) extends ContentValue {
  // The explicit construction of a wrapped integer here is needed to
  // keep overload resolution in scalac 2.8.1 from getting confused.
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Integer( value ))
}
case class CvLong( value: Long ) extends ContentValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Long( value ))
}
case class CvFloat( value: Float ) extends ContentValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Float( value ))
}
case class CvDouble( value: Double ) extends ContentValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Double( value ))
}

object ContentValue {
  implicit def intToContentValue( value: Int ):ContentValue = CvInt( value )
  implicit def longToContentValue( value: Long ):ContentValue = CvLong( value )
  implicit def booleanToContentValue( value: Boolean ) = CvBoolean( value )
  implicit def floatToContentValue( value: Float ) = CvFloat( value )
  implicit def doubleToContentValue( value: Double ) = CvDouble( value )
  implicit def stringToContentValue( value: String ):ContentValue = 
    CvString( value )
}

class DbQuery( source: ContentSource, 
               tableName: String,
               orderString: String = null,
               whereString: String = null,
               whereValues: Array[String] = null,
               limitString: String = null
             ) 
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

  def where( s: String, arr: Array[ContentValue] = null ):DbQuery = {

    val newValues = 
      if (arr == null) null
      else arr.map{ _.asConditionString }

    dinkedCopy(
      whereString =
        if (this.whereString == null) s
        else "(" + this.whereString + ") and (" + s + ")",
      whereValues =
        if (this.whereValues == null) newValues
        else if (arr == null || arr.length == 0) this.whereValues
        else this.whereValues ++ newValues
    )
  }

  def whereEq( pairs: (String, ContentValue)* ):DbQuery = {
    
    // Note that this conses up a *lot* of temporary objects.
    // In performance-sensitive contexts, directly invoking
    // "where" won't look as neat, but it may go faster.
    //
    // Then again, replacing whereString with a StringBuffer
    // would ameliorate a lot of the consing.

    val str = pairs.map{ "(" + _._1 + " = ?)" }.reduceLeft{ _ + " and " + _ }
    val vals = pairs.map{ _._2 }.toArray

    this.where( str, vals )

  }

  def buildContentValues( assigns: (String, ContentValue)* ):ContentValues = {
    val cv = new ContentValues
    for ( (key, sqlVal) <- assigns ) {
      sqlVal.putContentValues( cv, key )
    }
    return cv
  }

  def log( stmtType: String, 
           contentValues: ContentValues = null, 
           cols: Array[String]=null ) =
  {
    if (source.getLogTag != null) {
      val b = new StringBuffer(120)

      b.append( "DB: " ); 
      b.append( stmtType );  b.append( " " )
      b.append( tableName ); b.append( " " )

      if (whereString != null) {
        b.append( "where " ); b.append( whereString )
        if (whereValues != null) {
          b.append( "[ " )
          for (v <- whereValues) { 
            b.append( '"' ); b.append( v ); b.append( "\" " )
          }
          b.append( "] ")
        }
      }
      if (orderString != null) { 
        b.append( "order " ); b.append( orderString ); b.append( " " )
      }
      if (limitString != null) { 
        b.append( "limit " ); b.append( limitString ); b.append( " " )
      }
      if (contentValues != null) {
        b.append( "values ")
        val it = contentValues.valueSet.iterator
        while (it.hasNext()) {

          val entry = it.next()
          val entryVal = entry.getValue
          val entryValStr =(if (entryVal == null) "NULL" else entryVal.toString)

          b.append( entry.getKey ); b.append( "=" ); 
          b.append( entryValStr ); b.append(" ")
        }
      }
      if (cols != null) {
        b.append( "columns " )
        for( col <- cols ) { b.append( col ); b.append( " " ) }
      }
      Log.d( source.getLogTag, b.toString )
    }
  }

  def delete = {
    log( "delete" )
    source.delete( tableName, whereString, whereValues )
  }

  def update( assigns: (String, ContentValue)* ) = {
    val cv = buildContentValues( assigns:_* )
    log( "update", contentValues = cv )
    source.update( tableName, cv, whereString, whereValues )
  }

  def insert( assigns: (String, ContentValue)* ) = {
    val cv = buildContentValues( assigns:_* )
    log( "insert", contentValues = cv )
    source.insert( tableName, cv )
  }

  def select( cols: String* ) = {
    val colsArr = cols.toArray
    log( "select", cols = colsArr )
    val rawCursor = source.query( 
      tableName, colsArr, whereString, whereValues, null, null, 
      orderString, limitString )
    new PositronicCursor( rawCursor )
  }

  def oneRow( cols: String* ) = {
    val c = limit( 1 ).select( cols:_* )
  }

  def count:Long = {
    val c = this.select( "count(*)" );
    c.moveToFirst
    val result = c.getLong(0)
    c.close
    return result
  }
}

// Wrapper around cursors to support a proper 'foreach', so
// "for ( c <- myQuery.select(...))" works.
//
// These also implement the read-side of our Boolean conversions.

class PositronicCursor( wrappedCursor: android.database.Cursor )
  extends CursorWrapper( wrappedCursor )
{
  // Our extensions...

  def getBoolean( colIdx: Int ) = { getInt( colIdx ) != 0 }

  def foreach( func: PositronicCursor => Unit ):Unit = {
    moveToFirst
    while (! isAfterLast ) { func( this ); moveToNext }
    close
  }

  def map[T]( func: PositronicCursor => T ):IndexedSeq[T] = {
    var buf = new ArrayBuffer[T]
    moveToFirst
    while (! isAfterLast ) { buf += func( this ); moveToNext }
    close
    return buf
  }
}

// And the wrapper machinery itself.

class CursorWrapper( wrappedCursor: android.database.Cursor ) 
  extends android.database.Cursor
{
  // Provide a way to get the underlying object back, if 
  // somebody needs it ...

  def wrappedCursorAs[T] = wrappedCursor.asInstanceOf[ T ]

  // ... and delegate the full standard API.  (Including the 
  // deprecated bits.)

  // Lifecycle stuff
  
  def close = wrappedCursor.close
  def deactivate = wrappedCursor.deactivate
  def isClosed = wrappedCursor.isClosed
  def requery = wrappedCursor.requery

  // Properties of current data set

  def getCount = wrappedCursor.getCount
  def getPosition = wrappedCursor.getPosition

  def getColumnCount = wrappedCursor.getColumnCount
  def getColumnName( idx: Int ) = wrappedCursor.getColumnName( idx )
  def getColumnNames = wrappedCursor.getColumnNames
  def getColumnIndex( colName: String ) = 
    wrappedCursor.getColumnIndex( colName )
  def getColumnIndexOrThrow( colName: String ) =
    wrappedCursor.getColumnIndexOrThrow( colName )

  // Positioning.  

  def isAfterLast = wrappedCursor.isAfterLast
  def isBeforeFirst = wrappedCursor.isBeforeFirst
  def isFirst = wrappedCursor.isFirst
  def isLast = wrappedCursor.isLast
  def isNull( idx: Int ) = wrappedCursor.isNull( idx )
  def move( offset: Int ) = wrappedCursor.move( offset )
  def moveToFirst = wrappedCursor.moveToFirst
  def moveToLast  = wrappedCursor.moveToLast
  def moveToNext  = wrappedCursor.moveToNext
  def moveToPosition( offset: Int ) = wrappedCursor.moveToPosition( offset )
  def moveToPrevious = wrappedCursor.moveToPrevious

  // Getting contents out.

  def copyStringToBuffer( idx: Int, buf: CharArrayBuffer ) =
    wrappedCursor.copyStringToBuffer( idx, buf )
  def getBlob( idx: Int ) = wrappedCursor.getBlob( idx )
  def getDouble( idx: Int ) = wrappedCursor.getDouble( idx )
  def getFloat( idx: Int ) = wrappedCursor.getFloat( idx )
  def getInt( idx: Int ) = wrappedCursor.getInt( idx )
  def getLong( idx: Int ) = wrappedCursor.getLong( idx )
  def getShort( idx: Int ) = wrappedCursor.getShort( idx )
  def getString( idx: Int ) = wrappedCursor.getString( idx )
  // def getType( idx: Int ) = wrappedCursor.getType( idx )

  // Notifications and observers

  def setNotificationUri( cr: ContentResolver, uri: android.net.Uri ) =
    wrappedCursor.setNotificationUri( cr, uri )
  def registerContentObserver( observer: ContentObserver ) = 
    wrappedCursor.registerContentObserver( observer )
  def registerDataSetObserver( observer: DataSetObserver ) =
    wrappedCursor.registerDataSetObserver( observer )
  def unregisterContentObserver( observer: ContentObserver ) =
    wrappedCursor.unregisterContentObserver( observer )
  def unregisterDataSetObserver( observer: DataSetObserver ) =
    wrappedCursor.unregisterDataSetObserver( observer )

  // "Out-of-band" stuff, and similar oddities

  def getExtras = wrappedCursor.getExtras
  def respond( extras: Bundle ) = wrappedCursor.respond( extras )
  def getWantsAllOnMoveCalls = wrappedCursor.getWantsAllOnMoveCalls
}

// Generic interface to "content sources", including databases
// and (soon) content providers...

trait ContentSource {
  def delete( whence: String, where: String, whereArgs: Array[String] ): Int
  def update( whence: String, vals: ContentValues, 
              where: String, whereArgs: Array[ String ] ): Int
  def insert( where: String, vals: ContentValues ): Long
  def query( whence: String, cols: Array[ String ], 
             where: String, whereArgs: Array[ String ],
             groupBy: String, having: String,
             order: String, limit: String ): Cursor
  def getLogTag: String
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



