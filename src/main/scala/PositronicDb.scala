package org.positronic.db

import _root_.android.database.sqlite._
import _root_.android.content.ContentValues
import _root_.android.content.Context
import _root_.android.util.Log
import _root_.java.util.concurrent.BlockingQueue
import _root_.java.util.concurrent.LinkedBlockingQueue

// Mummery to make sure that on inserts and updates, strings and ints
// are added into contentValues objects with the appropriate types.
// We declare an "SqlValue" variant type, declare these things as
// taking ((String, SqlValue)*) arguments, and supply implicit
// conversions from the primitive types to SqlValue.
//
// The only point of this, right at the moment, is to handle the
// Boolean conversions --- SQLite has no native boolean type, and
// the convention is integer columns valued 0 and 1.  If we had just
// Strings and Ints to deal with, we wouldn't need any of this crud,
// as SQLite converts at need internally.  (And many, many apps
// using the native Android API are already relying on that, 
// since 'where' clause values if supplied are bound as strings,
// but frequently used for comparison to int-valued columns.)

abstract class SqlValue {

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
case class SqlString( value: String ) extends SqlValue {
  def asConditionString = value
  def putContentValues( cv:ContentValues, key:String ) = cv.put( key, value )
}
case class SqlBoolean( value: Boolean ) extends SqlValue {
  def intValue = if (value) 1 else 0
  def asConditionString = intValue.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put(key, new java.lang.Integer(intValue))
}
case class SqlInt( value: Int ) extends SqlValue {
  // The explicit construction of a wrapped integer here is needed to
  // keep overload resolution in scalac 2.8.1 from getting confused.
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Integer( value ))
}
case class SqlLong( value: Long ) extends SqlValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Long( value ))
}
case class SqlFloat( value: Float ) extends SqlValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Float( value ))
}
case class SqlDouble( value: Double ) extends SqlValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Double( value ))
}

object SqlValue {
  implicit def intToSqlValue( value: Int ):SqlValue = SqlInt( value )
  implicit def longToSqlValue( value: Long ):SqlValue = SqlLong( value )
  implicit def stringToSqlValue( value: String ):SqlValue = SqlString( value )
  implicit def booleanToSqlValue( value: Boolean ) = SqlBoolean( value )
  implicit def floatToSqlValue( value: Float ) = SqlFloat( value )
  implicit def doubleToSqlValue( value: Double ) = SqlDouble( value )
}

class StatementFragment( db: Database, 
                         tableName: String,
                         orderString: String = null,
                         whereString: String = null,
                         whereValues: Array[String] = null,
                         limitString: String = null
                       ) {

  def dinkedCopy( db: Database               = this.db, 
                  tableName: String          = this.tableName,
                  orderString: String        = this.orderString,
                  whereString: String        = this.whereString,
                  whereValues: Array[String] = this.whereValues,
                  limitString: String        = this.limitString ) =
    new StatementFragment( db, tableName, orderString, 
                           whereString, whereValues, limitString )

  def order( s: String ) = { dinkedCopy( orderString = s ) }
  def limit( s: String ) = { dinkedCopy( limitString = s ) }
  def limit( l: Int )    = { dinkedCopy( limitString = l.toString ) }

  def where( s: String, arr: Array[SqlValue] = null ):StatementFragment = {

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

  def whereEq( pairs: (String, SqlValue)* ):StatementFragment = {
    
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

  def buildContentValues( assigns: (String, SqlValue)* ):ContentValues = {
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
    if (db.getLogTag != null) {
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
          b.append( entry.getKey ); b.append( "=" ); 
          b.append( entry.getValue.toString ); b.append(" ")
        }
      }
      if (cols != null) {
        b.append( "columns " )
        for( col <- cols ) { b.append( col ); b.append( " " ) }
      }
      _root_.android.util.Log.d( db.getLogTag, b.toString )
    }
  }

  def delete = {
    log( "delete" )
    db.getWritableDatabase.delete( tableName, whereString, whereValues )
  }

  def update( assigns: (String, SqlValue)* ) = {
    val cv = buildContentValues( assigns:_* )
    log( "update", contentValues = cv )
    db.getWritableDatabase.update( tableName, cv, whereString, whereValues )
  }

  def insert( assigns: (String, SqlValue)* ) = {
    val cv = buildContentValues( assigns:_* )
    log( "insert", contentValues = cv )
    db.getWritableDatabase.insert( tableName, null, cv )
  }

  def select( cols: String* ) = {
    val colsArr = cols.toArray
    log( "select", cols = colsArr )
    db.getWritableDatabase.query( 
      tableName, colsArr, whereString, whereValues, null, null, 
      orderString, limitString ).asInstanceOf[ Cursor ]
  }

  def oneRow( cols: String* ) = {
    val c = limit( 1 ).select( cols:_* )
  }

}

// Arrange to produce cursors which support a proper 'foreach', so
// "for ( c <- myFrag.select(...))" works.
//
// These also implement the read-side of our Boolean conversions.

class Cursor( db: SQLiteDatabase,
              driver: SQLiteCursorDriver,
              table: String,
              query: SQLiteQuery )
 extends SQLiteCursor( db, driver, table, query ) {

   def getBoolean( colIdx: Int ) = { getInt( colIdx ) != 0 }

   def foreach( func: Cursor => Unit ):Unit = {
     moveToFirst
     while (! isAfterLast ) { func( this ); moveToNext }
     close
   }

}

class CursorFactory extends SQLiteDatabase.CursorFactory {
  def newCursor( db: SQLiteDatabase,
                 driver: SQLiteCursorDriver,
                 table: String,
                 query: SQLiteQuery ) = {
    new Cursor( db, driver, table, query )
  }
}

class DbWrapper( ctx: Context, mydb: Database ) 
extends SQLiteOpenHelper( ctx, mydb.getFilename, 
                          new CursorFactory, mydb.version ){

  def onCreate( db: SQLiteDatabase ) = mydb.onCreate( db )
  
  def onUpgrade( db: SQLiteDatabase, oldVersion: Int, newVersion: Int ) = 
    mydb.onUpgrade( db, oldVersion, newVersion )

}

abstract class Database( filename: String, logTag: String = null ) {

  var dbWrapper: DbWrapper = null
  var openNesting: Int = 0

  def schemaUpdates: List[String]

  def getLogTag = logTag
  def getFilename = filename

  def getWritableDatabase = dbWrapper.getWritableDatabase
  def getReadableDatabase = dbWrapper.getReadableDatabase
  
  def realOpen(ctx: Context) = { dbWrapper = new DbWrapper( ctx, this ) }
  def realClose              = { dbWrapper.close; dbWrapper = null }

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

  def onCreate( db: SQLiteDatabase ) = onUpgrade( db, 0, version )
  
  def onUpgrade( db: SQLiteDatabase, oldVersion: Int, newVersion: Int ) = {
    assert( version == newVersion )
    // XXX --- doesn't work for update/insert/delete; need to think
    // about data migrations.  Doing a regex match to see if the
    // first nonblank word is "insert", "update", etc. ought to do it.
    schemaUpdates.drop( oldVersion ).foreach { db.execSQL( _ ) }
  }

  def version = schemaUpdates.length

  def apply( table: String ) = new StatementFragment( this, table )

  def log( s: String ) = {
    if (logTag != null) Log.d( logTag, s )
  }
}

abstract class DatabaseWithThread( filename: String, logTag: String = null )
extends Database( filename, logTag ) {

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
            if (logTag != null) logTag else "DB",
            "Uncaught exception on DB Thread",
            ex)
        }
      }
    }
  }

  def runOnDbThread( func: => Unit ) = {
    q.put( () => func )
  }

  var theThread: QueueRunnerThread = null

  def assertOnDbThread( s: String ) {
    // Don't build the message unless we're going to throw the error...
    if ( Thread.currentThread != theThread ) {
      assert( Thread.currentThread == theThread, s + " called off DB Thread" )
    }
  }

  override def realOpen( ctx: Context ) = {
    super.realOpen( ctx )
    theThread = new QueueRunnerThread( q )
    theThread.start
  }

  override def realClose = {

    // We want the thread to exit *after* draining all its current
    // work.  So, we put a "please go away" marker on the queue, and
    // wait for it to drain...

    runOnDbThread { theThread.prepareToExit }
    theThread.join
    theThread = null
    super.realClose
  }
  
}
