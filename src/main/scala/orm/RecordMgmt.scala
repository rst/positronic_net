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

// Class that actually manages shuffling in-core records into and
// out of persistent storage.

abstract class RecordManager[ T <: ManagedRecord : ClassManifest ]( repository: ContentQuery[_,_] )
  extends ChangeManager( repository.facility )
  with BaseScope[T]
{
  // Producing a new object (to be populated with mapped data from a query). 
  // Note that the default implementation requires a niladic constructor 
  // to exist in bytecode, which will *not* be the case if there's a
  // with-args constructor that supplies defaults for all args (viz.
  // case classes).  For now, RecordManagers can override; for later,
  // the reflection to deal with this situation is possible, but painful.

  def newRecord = klass.newInstance.asInstanceOf[T]  // if niladic constructor exists!

  // Setting up the mapping of fields to storage columns.

  private val klass = classManifest[T].erasure
  private val javaFields = ReflectUtils.declaredFieldsByName( klass )

  private val fields = new mutable.ArrayBuffer[ MappedField ]
  private val nonKeyFields = new mutable.ArrayBuffer[ MappedField ]
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

    val idx = fields.size
    val mappedField = MappedField.create( columnName, idx, javaField )

    fields += mappedField
    if (!primaryKey) {
      nonKeyFields += mappedField
    }
    else {
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

  private lazy val fieldNames = fields.map{ _.name }

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

object ReflectUtils
{
  // Technique borrowed from sbt's ReflectUtilities, cut down to fit here.

  def ancestry( klass: Class[_] ): List[ Class[_]] =
    if (klass == classOf[ AnyRef ]) List( klass )
    else klass :: ancestry( klass.getSuperclass )

  def declaredFieldsByName( klass: Class[_] ) = {
    val fieldList = ancestry( klass ).flatMap( _.getDeclaredFields )
    Map( fieldList.map( f => (f.getName, f )): _* )
  }
}
