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

/** Abstract wrapper for any value that can be stored in a field
  * of a data source in a [[org.positronicnet.content.ContentRepository]] ---
  * currently, a SQLite database or Android ContentProvider.
  *
  * NB that there are implicit conversions in the ContentValue
  * companion object that coerce various primitive types to a
  * [[org.positronicnet.content.ContentValue]], to facilitate
  * their use with the [[org.postronicnet.content.ContentQuery]]
  * API, etc.  In addition to handling the usual primitives, this
  * also allows use of Booleans, adhering to the recommended
  * SQLite convention of converting them to an integer 0 (false)
  * or 1 (true).
  *
  * A further implementation note:
  * 
  * Android's Database API has two forms of binding:  in a lot
  * of places, you can stuff things into a contentValues, which
  * holds typed objects --- but for bound variables in where
  * clauses (the various selectionArgs arguments), you can supply
  * only strings.  Fortunately, it matters less than one might
  * think, since the SQLite engine does conversions if you ask
  * it to compare a number to a string.  But it does mean that
  * if you're implementing conversions of your own --- as we
  * do with Booleans here, to implement the standard SQLite
  * convention of representing them as 0 or 1 --- you have to
  * specify the conversions both ways to keep it consistent.
  */

abstract class ContentValue {

  /** Represent this ContentValue as a string, for use in the
    * `whereArgs` of various Android API calls.
    */

  def asConditionString: String

  /** Put this contentValue into an Android ContentValues object,
    * as the value of the given key.
    */

  def putContentValues( cv: ContentValues, key: String )
}

/** Strings as [[org.positronicnet.content.ContentValue]]s */

case class CvString( value: String ) extends ContentValue {
  def asConditionString = value
  def putContentValues( cv:ContentValues, key:String ) = cv.put( key, value )
}

/** ByteArrays as [[org.positronicnet.content.ContentValue]]s */

case class CvBlob( value: Array[Byte] ) extends ContentValue {
  def asConditionString = 
    throw new RuntimeException("Can't do equality comparisons to blobs!")
  def putContentValues( cv:ContentValues, key:String ) = cv.put( key, value )
}

/** Booleans as [[org.positronicnet.content.ContentValue]]s.
  * 
  * Implemented using the recommended SQLite convention that
  * false be represented by an integer 0, and true by an integer 1.
  */

case class CvBoolean( value: Boolean ) extends ContentValue {
  private val intValue = if (value) 1 else 0
  def asConditionString = intValue.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put(key, new java.lang.Integer(intValue))
}

/** Ints as [[org.positronicnet.content.ContentValue]]s */

case class CvInt( value: Int ) extends ContentValue {
  // The explicit construction of a wrapped integer here is needed to
  // keep overload resolution in scalac 2.8.1 from getting confused.
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Integer( value ))
}

/** Longs as [[org.positronicnet.content.ContentValue]]s */

case class CvLong( value: Long ) extends ContentValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Long( value ))
}

/** Floats as [[org.positronicnet.content.ContentValue]]s */

case class CvFloat( value: Float ) extends ContentValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Float( value ))
}

/** Doubles as [[org.positronicnet.content.ContentValue]]s */

case class CvDouble( value: Double ) extends ContentValue {
  def asConditionString = value.toString
  def putContentValues( cv:ContentValues, key:String ) = 
    cv.put( key, new java.lang.Double( value ))
}

/** Companion object for the [[org.positronicnet.org.ContentValue]] class.
  * Defines implicit conversions.
  */

object ContentValue {
  implicit def intToContentValue( value: Int ):ContentValue = CvInt( value )
  implicit def longToContentValue( value: Long ):ContentValue = CvLong( value )
  implicit def booleanToContentValue( value: Boolean ) = CvBoolean( value )
  implicit def floatToContentValue( value: Float ) = CvFloat( value )
  implicit def doubleToContentValue( value: Double ) = CvDouble( value )
  implicit def stringToContentValue( value: String ):ContentValue = 
    CvString( value )
  implicit def byteArrayToContentValue( value: Array[Byte] ):ContentValue = 
    CvBlob( value )
}

/** Generic interface to "content repositories", including databases
  * and ContentResolvers.  Each repository can host multiple "sources"
  * (tables in a database, ContentProviders for the content resolver,
  * etc.).  Ordinarily these methods aren't invoked directly, but instead
  * by means of a [[org.positronicnet.content.ContentQuery]] object, which
  * provides a shorthand fluid interface to adding conditions, etc.
  *
  * This trait, and the associated [[org.positronicnet.content.ContentQuery]]
  * class, are intended as a shorthand, along the lines of the Positronic Net
  * [[org.positronicnet.ui]] shorthand, which allows easier use of the APIs,
  * with less boilerplate code, but without changing any of the underlying
  * semantics much.
  *
  * The type parameters are the type of the handle to the underlying Android
  * objects (e.g., a `SQLiteDatabase`) and the type of the row IDs returned
  * by `insert`.  (Note that most of the
  * [[org.positronicnet.content.ContentRepository]] calls allow the
  * specification of a String `where` argument which can name a source with
  * more specificity, e.g., a table within a database.)
  *
  * For use with the ORM, the latter must be `Long`; thus, for
  * instance, the ORM can handle a
  * [[org.positronicnet.content.LongIdContentResolverRepository]] (which
  * assumes that the URIs returned by the underlying ContentResolver encode
  * an ID according to the usual assumptions), but not a
  * [[org.positronicnet.content.UriIdContentResolverRepository]] (which
  * assumes nothing).  See
  * [[org.positronicnet.content.PositronicContentResolver]] for more.
  */

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

/** Query on an arbitrary content source.  See the
  * [[org.positronicnet.content]] package overview for expected usage.
  *
  * Note that as an implementation cheat, this internally represents
  * query parameters (e.g., limitString) which not all ContentSources
  * will support.  We expect to stay safe because the publicly
  * exported subclasses that you get from the content sources won't
  * let users ''set'' the fields that the source can't support.
  */

abstract class ContentQuery[SourceType,IdType](
    source: ContentRepository[SourceType,IdType], 
    subSource: SourceType,
    orderString: String,
    val whereString: String,
    val whereValues: Array[String],
    limitString: String
  ) 
{
  if (source == null)
    throw new RuntimeException( getClass.getName + " with null source" )

  /** All conditions on this ContentQuery.  Useful to determine when
    * two are equivalent.
    */

  def conditionKey = (whereString, whereValues.toSeq)

  /** Returns a new ContentQuery with the same semantics as this one,
    * except that it adds extra conditions.  If the condition contains
    * '?' placeholders, they will be replaced with
    * [[org.positronicnet.content.ContentValue]] arguments, e.g.
    * {{{
    *     qry = otherQuery.where( "created_at > ? and status = ?",
    *                             myDate.getTime, "in_progress" )
    * }}}
    */

  def where( s: String, vals: ContentValue* ):ContentQuery[SourceType,IdType]

  /** Returns a new ContentQuery with the same semantics as this one,
    * except that it adds the condition that each of the fields named
    * by the `String`s in the arguments should be equal to the associated
    * [[org.positronicnet.content.ContentValue]].  E.g.,
    * {{{
    *     qry = otherQuery.where( "status" -> "in_progress",
    *                             "urgency_level" -> 3 )
    * }}}
    * using the same shorthand for pairs as is commonly used for
    * `Map` constructors.
    */

  def whereEq( pairs: (String, ContentValue)* ):ContentQuery[SourceType,IdType]

  protected
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

  protected
  def withUpdatedWhere[T]( pairs: Seq[(String, ContentValue)] )
                         ( handler: (String, Array[String]) => T ):T = 
  {
    val str = pairs.map{ "(" + _._1 + " = ?)" }.reduceLeft{ _ + " and " + _ }
    val vals = pairs.map{ _._2 }.toArray

    this.withUpdatedWhere( str, vals )( handler )
  }

  /** Returns a new `ContentQuery`, equivalent to this one, except
    * that results will be ordered as per the given `String`.
    * Interpretation of the `String`s is as per the underlying APIs,
    * e.g., as for a SQL `order by` when querying a database.
    */

  def order( s: String ): ContentQuery[SourceType, IdType]

  protected
  def buildContentValues( assigns: (String, ContentValue)* ):ContentValues = {
    val cv = new ContentValues
    for ( (key, sqlVal) <- assigns ) {
      sqlVal.putContentValues( cv, key )
    }
    return cv
  }

  protected
  def log( stmtType: String, 
           contentValues: ContentValues = null, 
           cols: Array[String]=null ) =
  {
    if (source.getLogTag != null) {
      val b = new StringBuffer(120)
      b.append( stmtType ); b.append( " " )
      this.describeIntoStringBuffer( b )
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

  override def toString = {
    val b = new StringBuffer(120)
    b.append( super.toString ); b.append(": ")
    this.describeIntoStringBuffer( b )
    b.toString
  }

  private def describeIntoStringBuffer( b: StringBuffer ): Unit = {

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
  }

  /** Returns a count of records matching this query's conditions.
    * May not be supported by all repositories; in particular, it is
    * not supported by ContentProviders.
    */

  def count:Long

  /** Returns a new query whose results will be limited to the given
    * number of rows.
    * May not be supported by all repositories; in particular, it is
    * not supported by ContentProviders.
    */

  def limit( s: String ): ContentQuery[ SourceType, IdType ]

  /** Returns a new query whose results will be limited to the given
    * number of rows.
    * May not be supported by all repositories; in particular, it is
    * not supported by ContentProviders.
    */

  def limit( l: Int ): ContentQuery[ SourceType, IdType ] = 
    limit( l.toString )

  /** Delete all records which match this ContentQuery */

  def delete = {
    log( "delete" )
    source.delete( subSource, whereString, whereValues )
  }

  /** Updates each record matched by the query, setting values of the columns
    * named by the `String`s to each associated
    * [[org.positronicnet.content.ContentValue]].  E.g.,
    * {{{
    *     qry.update( "status" -> "in_progress",
    *                 "urgency_level" -> 3 )
    * }}}
    * using the same shorthand for pairs as is commonly used for
    * `Map` constructors.
    */

  def update( assigns: (String, ContentValue)* ) = 
    updateFromContentValues( buildContentValues( assigns:_* ))

  /** As `update`, but with parameters from a standard Android `ContentValues`
    */

  def updateFromContentValues( cv: ContentValues ) = {
    log( "update", contentValues = cv )
    source.update( subSource, cv, whereString, whereValues )
  }

  /** Inserts a new record, with values of the columns
    * named by the `String`s to each associated
    * [[org.positronicnet.content.ContentValue]].  E.g.,
    * {{{
    *     qry.insert( "description" -> "write documentation",
    *                 "status" -> "in_progress",
    *                 "urgency_level" -> 3 )
    * }}}
    * using the same shorthand for pairs as is commonly used for
    * `Map` constructors.
    */

  def insert( assigns: (String, ContentValue)* ) = 
    insertFromContentValues( buildContentValues( assigns: _* ))

  def insertFromContentValues( cv: ContentValues ) = {
    log( "insert", contentValues = cv )
    source.insert( subSource, cv )
  }

  /** Return a [[org.positronicnet.content.PositronicCursor]] for the
    * values of the given columns, in records
    * matching this query, respecting its `order` and `limit`.
    *
    * (This is a wrapper around standard Android `Cursor`s, which provides
    * a few extra features; its `wrappedCursorAs` method can be used to
    * get the underlying raw Cursor, if you want it.)
    */

  def select( cols: String* ) = selectCols( cols.toArray )

  /** Return a [[org.positronicnet.content.PositronicCursor]] for the
    * values of the underlying resource's default set of columns, in records
    * matching this query, respecting its `order` and `limit`.
    *
    * (This is a wrapper around standard Android `Cursor`s, which provides
    * a few extra features; its `wrappedCursorAs` method can be used to
    * get the underlying raw Cursor, if you want it.)
    */

  def selectDefaultColumns    = selectCols( null )

  private def selectCols( colsArr: Array[ String ] ) = {
    log( "select", cols = colsArr )
    val rawCursor = source.query( 
      subSource, colsArr, whereString, whereValues, null, null, 
      orderString, limitString )
    new PositronicCursor( rawCursor )
  }

  def facility: org.positronicnet.facility.AppFacility
}

/** Wrapper around cursors to support `foreach` and `map`, so you can do, e.g.
  * {{{
  *     for ( c <- myQuery.select(...))
  *     yield new Frob( c.getString(0), c.getLong(1) ) 
  * }}}
  *
  * These also implement the read-side of our Boolean conversions.
  */

class PositronicCursor( wrappedCursor: android.database.Cursor )
  extends CursorWrapper( wrappedCursor )
{
  /** Get the value of the `colIdx`'th column, as a `Boolean` */

  def getBoolean( colIdx: Int ) = { getInt( colIdx ) != 0 }

  /** Starting at the first row covered by the cursor, proceeding to
    * the last, call `func` at each, supplying the cursor as an argument
    * so that the values of the columns are available.
    */

  def foreach( func: PositronicCursor => Unit ):Unit = {
    var hitLast = false                 // Work around robolectric bugs!
    moveToFirst
    while (! isAfterLast && ! hitLast ) { 
      hitLast = hitLast || ( getPosition == getCount - 1 )
      func( this ); moveToNext 
    }
    close
  }

  /** As `forEach`, but returns an `IndexedSeq` of the results.
    */

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

/** Wrapper around standard Android cursors, available as a base class
  * for extensions.
  *
  * `wrappedCursorAs` provides a way to get the underlying `Cursor`, if
  * you need it.
  */

class CursorWrapper( wrappedCursor: android.database.Cursor ) 
  extends android.database.Cursor
{
  if (wrappedCursor == null)
    throw new RuntimeException( "Can't wrap 'null' as a Cursor" )

  /** Provide a way to get the underlying object back, if 
    * somebody needs it ...
    */

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


