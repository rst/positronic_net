package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.util._

import android.database.Cursor

import scala.collection._

// Trait for objects that will be persisted by this ORM into some
// ContentRepository (be it a SQLiteDatabase, a ContentProvider, or whatever).
//
// Most of the heavy lifting is delegated to a RecordManager singleton,
// for which see below.
//
// Note that the manager MUST be a RecordManager[ thisclass ], but it's
// awfully awkward to write that constraint...

abstract class ManagedRecord( private[orm] val manager: RecordManager[_] ) {

  // Bookkeeping

  private [orm] var unsaved = true

  val id: Long

  def isNewRecord = (id == ManagedRecord.unsavedId)
  def isUnsaved   = unsaved

  // Actions on individual records, delegated to the manager...

  def delete = manager.delete( this )
  def save   = manager.save( this )

  def deleteOnThisThread = manager.deleteOnThisThread( this )
  def saveOnThisThread   = manager.saveOnThisThread( this )
}

object ManagedRecord {
  val unsavedId = -1
}

// Base class for management of shuffling in-core records into and
// out of persistent storage.

abstract class BaseRecordManager[ T <: ManagedRecord : ClassManifest ]( repository: ContentQuery[_,_] )
  extends ChangeManager( repository.facility )
  with BaseScope[T]
{
  // Producing a new object (to be populated with mapped data from a query). 
  // Note that the default implementation requires a niladic constructor 
  // to exist in bytecode, which will *not* be the case if there's a
  // with-args constructor that supplies defaults for all args (viz.
  // case classes).  
  //
  // Default is to use a niladic constructor if one exists.  Failing that,
  // if there is *one* constructor, and defaults for all its arguments,
  // we'll use that.  (See ReflectUtils.getObjectBuilder for details.)
  //
  // In other cases, RecordManagers can override.

  def newRecord = builder()
  private lazy val builder = ReflectUtils.getObjectBuilder[ T ] 

  // Setting up the mapping of fields to storage columns.
  //
  // Basics here to explicitly manipulate the list within the
  // constructor.  The mapping is frozen at first use when the
  // lazy val 'fields' is computed below...

  protected[orm] val managedKlass = classManifest[T].erasure
  protected[orm] val javaFields = 
    ReflectUtils.declaredFieldsByName( managedKlass )

  protected[orm] val fieldsBuffer = new mutable.ArrayBuffer[ MappedField ]
  protected[orm] def fieldsSeq: Seq[ MappedField ] = fieldsBuffer

  private var primaryKeyField: MappedLongField = null

  def mapField( fieldName: String, 
                columnName: String, 
                primaryKey: Boolean = false ): Unit = 
  {
    val javaField = javaFields( fieldName )

    if (javaField == null) {
      throw new IllegalArgumentException( "Can't map nonexistent field " 
                                          + fieldName )
    }

    val idx = fieldsBuffer.size
    val mappedField = MappedField.create( columnName, idx, javaField )

    fieldsBuffer += mappedField
    
    if (primaryKey) {
      if (primaryKeyField != null) {
        throw new IllegalArgumentException( "Multiple primary key fields" )
      }
      mappedField match {
        case f: MappedLongField => 
          primaryKeyField = f
        case _ => 
          throw new IllegalArgumentException("Primary key field must be Long")
      }
    }
  }

  // Feeding the Scope machinery what it needs

  private [orm] val facility = repository.facility
  private [orm] val mgr = this
  private [orm] val baseQuery = repository

  // Dealing with the data... internals

  protected [orm] lazy val fields = fieldsSeq
  protected [orm] lazy val nonKeyFields = 
    fields.filter{ primaryKeyField == null || 
                   _.dbColumnName != primaryKeyField.dbColumnName }

  private lazy val fieldNames = fields.map{ _.dbColumnName }

  private [orm]
  def rawQuery( qry: ContentQuery[_,_] ): Seq[ T ] = {
    qry.select( fieldNames: _* ).map{ c => instantiateFrom( c ) }
  }

  private [orm] 
  def instantiateFrom( c: Cursor ): T = {
    val result = newRecord
    result.unsaved = false              // ... not yet altered.
    for( field <- fields ) field.setFromCursorColumn( result, c )
    return result
  }

  private [orm]
  def save( rec: ManagedRecord ) = doChange { saveOnThisThread( rec ) }

  private [orm]
  def saveOnThisThread( rec: ManagedRecord ) = {
    val data = nonKeyFields.map{ f => f.valPair( rec ) }
    if (rec.isNewRecord) 
      repository.insert( data: _* )
    else
      repository.whereEq( primaryKeyField.valPair( rec )).update( data: _* )
  }

  private [orm]
  def delete( rec: ManagedRecord ): Unit = 
    whereEq( primaryKeyField.valPair( rec )).deleteAll

  private [orm]
  def deleteOnThisThread( rec: ManagedRecord ): Unit = 
    whereEq( primaryKeyField.valPair( rec )).deleteAllOnThisThread
}

abstract class RecordManager[ T <: ManagedRecord : ClassManifest ]( repository: ContentQuery[_,_] )
  extends BaseRecordManager[ T ]( repository )
{
  override protected[orm] def fieldsSeq: Seq[ MappedField ] = {

    // Take explicit mappings, and add them to mappings for
    // like-named columns available from our repository.
    // For that, we have to do a dummy query which will
    // (hopefully) return no rows...

    val dummyCursor = repository.where( "2 + 2 = 5" ).selectDefaultColumns
    val dbColNames  = dummyCursor.getColumnNames
    val klassName   = managedKlass.getName

    for ( i <- Range( 0, dbColNames.length )) {

      val dbColName       = dbColNames( i ).toLowerCase
      val recordFieldName = 
        if (dbColName == "_id") "id" else camelize( dbColName )

      val javaField = javaFields( recordFieldName )

      val existingField = 
        fieldsBuffer.find(mappedField => 
            mappedField.dbColumnName == dbColName
            || mappedField.recordFieldName == recordFieldName )

      existingField match {
        case None => null
        case Some(x) => 
          repository.log( "=== For " + managedKlass.getName +
                          " found existing mapping for " + dbColName + 
                          " db name " + x.dbColumnName + 
                          " rec name " + x.recordFieldName )
                              
      }

      if (javaField != null &&
          ! fieldsBuffer.exists( mappedField => 
            mappedField.dbColumnName == dbColName
            || mappedField.recordFieldName == recordFieldName ))
      {
        // Try to map automatically
        
        repository.log( "=== For " + managedKlass.getName +
                        " attempting to map " + dbColName + " to " +
                        recordFieldName )

        mapField( recordFieldName, dbColName, dbColName == "_id" )
      }
    }

    dummyCursor.close
    return super.fieldsSeq
  }

  def camelize( str: String ) = str.split("_").reduceLeft{ _ + _.capitalize }
}
