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

  def create( colName: String, colNumber: Int, rfield: java.lang.reflect.Field )
    : MappedField =
  {
    val mapBuilder = declaredMappers( rfield.getType )
    if (mapBuilder == null) {
      throw new IllegalArgumentException( "Don't know how to map " 
                                          + rfield.getName
                                          + " of type " 
                                          + rfield.getType.toString )
    }
    return mapBuilder( colName, colNumber, rfield )
  }
}

abstract class MappedField( colName: String, 
                            colNumber: Int,
                            rfield: java.lang.reflect.Field )
{
  // serializers, ORMs, etc., can use "setAccessible( true )" to
  // bypass private access restrictions.  We're an ORM...

  rfield.setAccessible( true )

  val dbColumnName    = colName
  val recordFieldName = rfield.getName

  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit
  
  def getValue( o: AnyRef ): ContentValue

  def valPair( o: AnyRef ) = ( colName, getValue( o ))
}

class MappedIntField( colName: String, 
                      colNumber: Int,
                      rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setInt( o, c.getInt( colNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvInt( rfield.getInt( o ))
}

class MappedLongField( colName: String, 
                       colNumber: Int,
                       rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setLong( o, c.getLong( colNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvLong( rfield.getLong( o ))
}

class MappedFloatField( colName: String, 
                        colNumber: Int,
                        rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setFloat( o, c.getFloat( colNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvFloat( rfield.getFloat( o ))
}

class MappedDoubleField( colName: String, 
                         colNumber: Int,
                         rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setDouble( o, c.getDouble( colNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvDouble( rfield.getDouble( o ))
}

class MappedStringField( colName: String, 
                         colNumber: Int,
                         rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.set( o, c.getString( colNumber ))
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvString( rfield.get( o ).asInstanceOf[ String ] )
}

class MappedBooleanField( colName: String, 
                          colNumber: Int,
                          rfield: java.lang.reflect.Field )
  extends MappedField( colName, colNumber, rfield )
{
  def setFromCursorColumn( o: AnyRef, c: Cursor ): Unit = 
    rfield.setBoolean( o, c.getInt( colNumber ) != 0 )
  
  def getValue( o: AnyRef ): ContentValue = 
    new CvBoolean( rfield.getBoolean( o ))
}
