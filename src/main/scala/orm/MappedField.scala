package org.positronicnet.orm

import org.positronicnet.content._

import android.database.Cursor

import scala.collection._

// Code to manage mapping of fields to ContentValues.
//
// Makes extremely heavy use of Java reflection.  Which isn't ideal,
// but the obvious alternatives involve code generation at some point.
// Runtime generation of Dalvik bytecode is a really messy prospect;
// generating DAO implementations at compile time involves less 
// outright voodoo, but would mean getting rather deeply in bed with
// the build system, and there just *might* be someone out there who
// feels the same way about sbt as I feel about Eclipse.

/** Some content providers have fields in their pseudo-tables which
  * are effectively read-only, or write-once.  (They may, for instance,
  * be produced by joins on underlying tables, or export state that is
  * managed by the content provider itself.  Or, as in the case of
  * references to aggregated contacts from the ContactsContract stuff,
  * both.)  The ORM can be asked to request these constraints when
  * setting up a [[org.positronicnet.orm.RecordManager]], by calling
  * `mapField` with a non-default value of the `how` argument, chosen
  * from this enumeration...
  */

object MapAs extends Enumeration {
  type How = Value

  /** Normal case of an update-able field */
  val ReadWrite = Value

  /** Field is read, but never set.
    * 
    * Default for primary key/'id' fields, which we expect the DB to
    * assign on its own.
    */
  val ReadOnly = Value

  /** Field is set once, at insert, but untouched by updates. */

  val WriteOnce = Value
}

private [orm]
object MappedField {

  private [orm]
  val declaredMappers = 
    new mutable.HashMap[ java.lang.Class[_], 
                         (( String, Int, MapAs.How, java.lang.reflect.Field ) =>
                           MappedField) ]

  def declareMapper( klass: java.lang.Class[_], 
                     builder: (( String, Int, MapAs.How,
                                 java.lang.reflect.Field ) =>
                               MappedField )) =
    declaredMappers( klass ) = builder

  declareMapper( java.lang.Boolean.TYPE,
                 ( s, n, r, f ) => new MappedBooleanField( s, n, r, f ))
  declareMapper( java.lang.Integer.TYPE,
                 ( s, n, r, f ) => new MappedIntField( s, n, r, f ))
  declareMapper( java.lang.Long.TYPE,
                 ( s, n, r, f ) => new MappedLongField( s, n, r, f ))
  declareMapper( java.lang.Float.TYPE,
                 ( s, n, r, f ) => new MappedFloatField( s, n, r, f ))
  declareMapper( java.lang.Double.TYPE,
                 ( s, n, r, f ) => new MappedDoubleField( s, n, r, f ))
  declareMapper( classOf[ String ], 
                 ( s, n, r, f ) => new MappedStringField( s, n, r, f ))
  declareMapper( classOf[ Array[Byte] ], 
                 ( s, n, r, f ) => new MappedByteArrayField( s, n, r, f ))
  declareMapper( classOf[ RecordId[_] ],
                 ( s, n, r, f ) => new MappedIdField( s, n, r, f ))

  def create( colName: String, colNumber: Int, how: MapAs.How,
              rfield: java.lang.reflect.Field )
    : MappedField =
  {
    declaredMappers.get( rfield.getType ) match {
      case Some( mapBuilder ) => mapBuilder( colName, colNumber, how, rfield )
      case None =>
        throw new IllegalArgumentException( "Don't know how to map " 
                                           + rfield.getName
                                           + " of type " 
                                           + rfield.getType.toString )
    }
  }
}

@cloneable
private [orm]
abstract class MappedField( colName: String, 
                            colNumber: Int,
                            how: MapAs.How,
                            rfield: java.lang.reflect.Field )
{
  // serializers, ORMs, etc., can use "setAccessible( true )" to
  // bypass private access restrictions.  We're an ORM...

  rfield.setAccessible( true )

  val dbColumnName    = colName
  val recordFieldName = rfield.getName
  val recordField     = rfield
  var realColNumber   = colNumber       // may be reset by atIndex...
  val mappedHow       = how

  override def toString = 
    super.toString + "(" + colName + "," + recordFieldName + "," + how + ")"

  def atIndex( newColNumber: Int ) = {
    val result = this.clone.asInstanceOf[ MappedField ]
    result.realColNumber = newColNumber
    result
  }

  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit
  
  def getValue( o: AnyRef ): ContentValue

  def setValue( o: AnyRef, cv: ContentValue ): Unit

  def valPair( o: AnyRef ) = ( colName, getValue( o ))
}

private [orm]
class MappedIntField( colName: String, 
                      colNumber: Int,
                      how: MapAs.How,
                      rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setInt( o, c.getInt( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvInt( rfield.getInt( o ))

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.setInt( o, l.asInstanceOf[ CvInt ].value )
}

private [orm]
class MappedLongField( colName: String, 
                       colNumber: Int,
                       how: MapAs.How,
                       rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setLong( o, c.getLong( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvLong( rfield.getLong( o ))

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.setLong( o, l.asInstanceOf[ CvLong ].value )
}

private [orm]
class MappedFloatField( colName: String, 
                        colNumber: Int,
                        how: MapAs.How,
                        rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setFloat( o, c.getFloat( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvFloat( rfield.getFloat( o ))

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.setFloat( o, l.asInstanceOf[ CvFloat ].value )
}

private [orm]
class MappedDoubleField( colName: String, 
                         colNumber: Int,
                         how: MapAs.How,
                         rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setDouble( o, c.getDouble( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvDouble( rfield.getDouble( o ))

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.setDouble( o, l.asInstanceOf[ CvDouble ].value )
}

private [orm]
class MappedStringField( colName: String, 
                         colNumber: Int,
                         how: MapAs.How,
                         rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.set( o, c.getString( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvString( rfield.get( o ).asInstanceOf[ String ] )

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.set( o, l.asInstanceOf[ CvString ].value )
}

private [orm]
class MappedByteArrayField( colName: String, 
                            colNumber: Int,
                            how: MapAs.How,
                            rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.set( o, c.getBlob( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvBlob( rfield.get( o ).asInstanceOf[ Array[Byte] ] )

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.set( o, l.asInstanceOf[ CvBlob ].value )
}

private [orm]
class MappedBooleanField( colName: String, 
                          colNumber: Int,
                          how: MapAs.How,
                          rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setBoolean( o, c.getInt( realColNumber ) != 0 )
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvBoolean( rfield.getBoolean( o ))

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.setBoolean( o, l.asInstanceOf[ CvBoolean ].value )
}

private [orm]
object MappedIdField {
  val idField = classOf[RecordId[_]].getDeclaredField("id")
  val savedIdField = classOf[RecordId[_]].getDeclaredField("savedId")
  idField.setAccessible( true )
  savedIdField.setAccessible( true )
}

private [orm]
class MappedIdField( colName: String, 
                     colNumber: Int,
                     how: MapAs.How,
                     rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, how, rfield )
{
  // Wrapping up Longs in our RecordId objects.
  //
  // We assume that the constructor has already produced a recordId
  // object, and just dink that.

  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = {
    val recordIdObj = rfield.get( o ).asInstanceOf[ RecordId[_] ]
    val newId = c.getLong( realColNumber )

    // Set both 'id' and 'savedId' fields.  What I get for having them both,
    // I guess...

    MappedIdField.idField.set( recordIdObj, newId )
    recordIdObj.markSaved( newId )
  }
  
  def getValue( o: AnyRef ): ContentValue = {
    val recordIdObj = rfield.get( o )
    new CvLong( MappedIdField.savedIdField.getLong( recordIdObj ) )
  }

  def setValue( o: AnyRef, l: ContentValue ): Unit = {
    val recordIdObj = rfield.get( o )
    MappedIdField.idField.setLong( recordIdObj, l.asInstanceOf[ CvLong ].value )
  }
}

