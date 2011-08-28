package org.positronicnet.content

import _root_.android.database.Cursor
import _root_.android.os.Bundle

import _root_.android.database.ContentObserver
import _root_.android.database.DataSetObserver
import _root_.android.database.CharArrayBuffer

import _root_.android.content.ContentValues
import _root_.android.content.ContentResolver

import _root_.android.util.Log

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

// Generic interface to "content repositories", including databases
// and content providers.  Each repository can host multiple "sources"
// (tables in a database, ContentProviders for the content resolver,
// etc.).  Also generic in the IdType returned by an 'insert'.

trait ContentRepository[ SourceType, IdType ] {
  def delete( whence: SourceType, where: String, whereArgs: Array[String] ): Int
  def update( whence: SourceType, vals: ContentValues, 
              where: String, whereArgs: Array[ String ] ): Int
  def insert( where: SourceType, vals: ContentValues ): IdType
  def query( whence: SourceType, cols: Array[ String ], 
             where: String, whereArgs: Array[ String ],
             groupBy: String, having: String,
             order: String, limit: String ): Cursor
  def getLogTag: String
}

// Query on an arbitrary content source, with no syntactic sugar.
// Note that as an implementation cheat, this internally represents
// query parameters (e.g., limitString) which not all ContentSources
// will support.  We stay safe because the publicly exported subclasses
// that you get from the content sources won't let users *set* the 
// fields that the source can't support.

abstract class ContentQuery[SourceType,IdType](
    source: ContentRepository[SourceType,IdType], 
    subSource: SourceType,
    orderString: String,
    whereString: String,
    whereValues: Array[String],
    limitString: String
  ) 
{
  def conditionKey = (whereString, whereValues.toSeq)

  def where( s: String, arr: Array[ContentValue] = null ):ContentQuery[SourceType,IdType]

  def whereEq( pairs: (String, ContentValue)* ):ContentQuery[SourceType,IdType]

  def withUpdatedWhere[T]( s: String, arr: Array[ContentValue] )
                         ( handler: (String, Array[String]) => T ):T =
  {
    val addingValues = 
      if (arr == null) null
      else arr.map{ _.asConditionString }

    val newString = 
      if (this.whereString == null) s
      else "(" + this.whereString + ") and (" + s + ")"

    val newVals =
      if (this.whereValues == null) addingValues
      else if (arr == null || arr.length == 0) this.whereValues
      else this.whereValues ++ addingValues

    handler( newString, newVals )
  }

  def withUpdatedWhere[T]( pairs: Seq[(String, ContentValue)] )
                         ( handler: (String, Array[String]) => T ):T = 
  {
    val str = pairs.map{ "(" + _._1 + " = ?)" }.reduceLeft{ _ + " and " + _ }
    val vals = pairs.map{ _._2 }.toArray

    this.withUpdatedWhere( str, vals )( handler )
  }

  def order( s: String ): ContentQuery[SourceType, IdType]

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
      b.append( subSource.toString ); b.append( " " )

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

  def count:Long

  def limit( s: String ): ContentQuery[ SourceType, IdType ]
  def limit( l: Int ): ContentQuery[ SourceType, IdType ] = 
    limit( l.toString )

  def delete = {
    log( "delete" )
    source.delete( subSource, whereString, whereValues )
  }

  def update( assigns: (String, ContentValue)* ) = {
    val cv = buildContentValues( assigns:_* )
    log( "update", contentValues = cv )
    source.update( subSource, cv, whereString, whereValues )
  }

  def insert( assigns: (String, ContentValue)* ) = {
    val cv = buildContentValues( assigns:_* )
    log( "insert", contentValues = cv )
    source.insert( subSource, cv )
  }

  def select( cols: String* ) = selectCols( cols.toArray )
  def selectDefaultColumns    = selectCols( null )

  private def selectCols( colsArr: Array[ String ] ) = {
    log( "select", cols = colsArr )
    val rawCursor = source.query( 
      subSource, colsArr, whereString, whereValues, null, null, 
      orderString, limitString )
    new PositronicCursor( rawCursor )
  }

  def facility: org.positronicnet.util.AppFacility
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
    var hitLast = false                 // Work around robolectric bugs!
    moveToFirst
    while (! isAfterLast && ! hitLast ) { 
      hitLast = hitLast || ( getPosition == getCount - 1 )
      func( this ); moveToNext 
    }
    close
  }

  def map[T]( func: PositronicCursor => T ):IndexedSeq[T] = {
    var buf = new ArrayBuffer[T]
    if (getCount > 0) {
      var hitLast = false             // Work around robolectric bugs!
      moveToFirst
      while (! isAfterLast && ! hitLast ) { 
        hitLast = hitLast || ( getPosition == getCount - 1 )
        buf += func( this ); moveToNext 
      }
      close
    }
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


