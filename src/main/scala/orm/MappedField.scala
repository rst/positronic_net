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

private [orm]
object MappedField {

  private [orm]
  val declaredMappers = 
    new mutable.HashMap[ java.lang.Class[_], 
                         (( String, Int, java.lang.reflect.Field ) => 
                           MappedField) ]

  def declareMapper( klass: java.lang.Class[_], 
                     builder: (( String, Int, java.lang.reflect.Field ) =>
                               MappedField )) =
    declaredMappers( klass ) = builder

  declareMapper( java.lang.Boolean.TYPE,
                 ( s, n, f ) => new MappedBooleanField( s, n, f ))
  declareMapper( java.lang.Integer.TYPE,
                 ( s, n, f ) => new MappedIntField( s, n, f ))
  declareMapper( java.lang.Long.TYPE,
                 ( s, n, f ) => new MappedLongField( s, n, f ))
  declareMapper( java.lang.Float.TYPE,
                 ( s, n, f ) => new MappedFloatField( s, n, f ))
  declareMapper( java.lang.Double.TYPE,
                 ( s, n, f ) => new MappedDoubleField( s, n, f ))
  declareMapper( classOf[ String ], 
                 ( s, n, f ) => new MappedStringField( s, n, f ))
  declareMapper( classOf[ RecordId[_] ],
                 ( s, n, f ) => new MappedIdField( s, n, f ))

  def create( colName: String, colNumber: Int, rfield: java.lang.reflect.Field )
    : MappedField =
  {
    declaredMappers.get( rfield.getType ) match {
      case Some( mapBuilder ) => mapBuilder( colName, colNumber, rfield )
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
                            rfield: java.lang.reflect.Field )
{
  // serializers, ORMs, etc., can use "setAccessible( true )" to
  // bypass private access restrictions.  We're an ORM...

  rfield.setAccessible( true )

  val dbColumnName    = colName
  val recordFieldName = rfield.getName
  var realColNumber   = colNumber       // reset by atIndex...

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
                      rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
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
                       rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
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
                        rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
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
                         rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
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
                         rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.set( o, c.getString( realColNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvString( rfield.get( o ).asInstanceOf[ String ] )

  def setValue( o: AnyRef, l: ContentValue ): Unit = 
    rfield.set( o, l.asInstanceOf[ CvString ].value )
}

private [orm]
class MappedBooleanField( colName: String, 
                          colNumber: Int,
                          rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
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
  idField.setAccessible( true )
}

private [orm]
class MappedIdField( colName: String, 
                     colNumber: Int,
                     rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  // Wrapping up Longs in our RecordId objects.
  //
  // We assume that the constructor has already produced a recordId
  // object, and just dink that.

  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = {
    val recordIdObj = rfield.get( o )
    MappedIdField.idField.set( recordIdObj, c.getLong( realColNumber ))
  }
  
  def getValue( o: AnyRef ): ContentValue = {
    val recordIdObj = rfield.get( o )
    new CvLong( MappedIdField.idField.getLong( recordIdObj ) )
  }

  def setValue( o: AnyRef, l: ContentValue ): Unit = {
    val recordIdObj = rfield.get( o )
    MappedIdField.idField.setLong( recordIdObj, l.asInstanceOf[ CvLong ].value )
  }
}

