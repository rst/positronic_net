package org.triplesec.db

import _root_.android.database.sqlite._
import _root_.android.database._
import _root_.android.content.ContentValues
import _root_.android.content.Context

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

abstract class SqlValue
case class SqlInt( value: Int ) extends SqlValue
case class SqlLong( value: Long ) extends SqlValue
case class SqlString( value: String ) extends SqlValue
case class SqlBoolean( value: Boolean ) extends SqlValue

object SqlValue {
  implicit def intToSqlValue( value: Int ):SqlValue = SqlInt( value )
  implicit def longToSqlValue( value: Long ):SqlValue = SqlLong( value )
  implicit def stringToSqlValue( value: String ):SqlValue = SqlString( value )
  implicit def booleanToSqlValue( value: Boolean ) = SqlBoolean( value )
}

class StatementFragment( db: Database, tableName: String ) {

  var orderString: String = null
  var whereString: String = null
  var whereValues: Array[String] = null
  var limitString: String = null

  def order( s: String ): StatementFragment = {
    this.orderString = s
    return this
  }

  def limit( l: Int ): StatementFragment = {
    this.limitString = l.toString
    return this
  }

  def where( s: String ): StatementFragment = {
    this.whereString = s
    this.whereValues = null
    return this
  }

  def where( s: String, arr: Array[String] ): StatementFragment = {
    this.whereString = s
    this.whereValues = arr
    return this
  }

  // Implements stmt.where( col -> value, col -> value, ... )
  //
  // Note that the values all get bound as strings.  For integer-valued
  // columns, it doesn't matter much --- SQLite will convert back to
  // a number.  For dates, and other such exotica, you could get burned.
  // (The platform API, which we're using under the covers, has the same
  // glitch.)

  def where( equalities: ( String, Any )* ): StatementFragment = {
    if ( equalities.length > 0 ) {
      val clauses = for ( pair <- equalities ) yield "(" + pair._1 + " = ?)"
      val values = for ( pair <- equalities ) yield pair._2.toString
      this.whereString = clauses.toIndexedSeq.reduceLeft {(x,y) => x+" and "+y }
      this.whereValues = values.toArray
    }
    return this
  }

  def delete = {
    db.getWritableDatabase.delete( tableName, whereString, whereValues )
  }

  def update( assigns: (String, SqlValue)* ) = {
    db.getWritableDatabase.update( tableName, buildContentValues( assigns:_* ),
                                   whereString, whereValues )
  }

  def insert( assigns: (String, SqlValue)* ) = {
    db.getWritableDatabase.insert( tableName, null, 
                                   buildContentValues( assigns:_* ))
  }

  def buildContentValues( assigns: (String, SqlValue)* ):ContentValues = {
    val cv = new ContentValues
    for ( (col, sqlVal) <- assigns ) {
      sqlVal match {

        // Explicitly constructing java.lang.Integers here because scalac 2.8.1
        // gets confused about the overloadings; this helps it out.

        case SqlString( value ) => cv.put( col, value )
        case SqlInt( value )    => cv.put( col, new java.lang.Integer(value) )
        case SqlLong( value )   => cv.put( col, new java.lang.Long(value) )
        case SqlBoolean( value:Boolean ) => 
          cv.put( col, new java.lang.Integer( if (value) 1 else 0 ))
      }
    }
    return cv
  }

  def cursor( cols: String* ) = {
    db.getWritableDatabase.query( 
      tableName, cols.toArray, whereString, whereValues, null, null, 
      orderString, limitString ).asInstanceOf[ CursorWithGetBoolean ]
  }

  def eachRow( cols: String* )( func: CursorWithGetBoolean => Unit ) = {
    val c = cursor( cols:_* )
    c.moveToFirst
    while (! c.isAfterLast ) { func( c ); c.moveToNext }
    c.close
  }

  def oneRow( cols: String* )( func: CursorWithGetBoolean => Unit ) = {
    val c = limit( 1 ).cursor( cols:_* )
    c.moveToFirst
    func( c )
    c.close
  }

}

// Arrange to produce cursors with a getBoolean method....

class CursorWithGetBoolean( db: SQLiteDatabase,
                            driver: SQLiteCursorDriver,
                            table: String,
                            query: SQLiteQuery )
 extends SQLiteCursor( db, driver, table, query ) {

   def getBoolean( colIdx: Int ) = { getInt( colIdx ) != 0 }
}

class CursorFactory extends SQLiteDatabase.CursorFactory {
  def newCursor( db: SQLiteDatabase,
                 driver: SQLiteCursorDriver,
                 table: String,
                 query: SQLiteQuery ) = {
    new CursorWithGetBoolean( db, driver, table, query )
  }
}

class DbWrapper( ctx: Context, mydb: Database ) 
extends SQLiteOpenHelper( ctx, mydb.filename, new CursorFactory, mydb.version ){

  def onCreate( db: SQLiteDatabase ) = mydb.onCreate( db )
  
  def onUpgrade( db: SQLiteDatabase, oldVersion: Int, newVersion: Int ) = 
    mydb.onUpgrade( db, oldVersion, newVersion )

}

abstract class Database {

  var dbWrapper: DbWrapper = null
  var openNesting: Int = 0

  def schemaUpdates: List[String]
  def filename: String

  def getWritableDatabase = dbWrapper.getWritableDatabase
  def getReadableDatabase = dbWrapper.getReadableDatabase
  
  def openInContext( ctx: Context ) = {
    if (openNesting == 0) {
      dbWrapper = new DbWrapper( ctx, this )
    }
    openNesting += 1
  }

  def close = {
    openNesting -= 1
    if (openNesting == 0) {
      dbWrapper.close
      dbWrapper = null
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

}
