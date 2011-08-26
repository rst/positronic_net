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

  class HasMany[T <: ManagedRecord]( src: RecordManager[T], foreignKey: String )
    extends HasManyAssociation( src, foreignKey, this.id )
  {
    def this( src: RecordManager[T] ) = 
      this( src, src.columnNameFor( manager.defaultForeignKeyField ))
  }
}

object ManagedRecord {
  val unsavedId = -1
}

// Base class for management of shuffling in-core records into and
// out of persistent storage.

abstract class BaseRecordManager[ T <: ManagedRecord : ClassManifest ]( repository: ContentQuery[_,_] )
  extends BaseNotificationManager( repository.facility )
  with Scope[T]
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

  protected[orm] val managedKlass: Class[T] = 
    classManifest[T].erasure.asInstanceOf[ Class[T] ]

  protected[orm] val javaFields = 
    ReflectUtils.declaredFieldsByName( managedKlass )

  protected[orm] val fieldsBuffer = new mutable.ArrayBuffer[ MappedField ]
  protected[orm] def fieldsSeq: Seq[ MappedField ] = fieldsBuffer

  private var primaryKeyField: MappedLongField = null

  protected[orm] def columnNameFor( fieldName: String ) =
    fields.find{ _.recordFieldName == fieldName } match {
      case Some( mappedField ) => mappedField.dbColumnName
      case None =>
        throw new RuntimeException("Can't find mapping for field " + fieldName)
    }

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

  private [orm] val mgr = this

  val facility = repository.facility
  val baseQuery = repository

  // Dealing with the mappings... internals

  protected [orm] lazy val fields = fieldsSeq
  protected [orm] lazy val nonKeyFields = 
    fields.filter{ primaryKeyField == null || 
                   _.dbColumnName != primaryKeyField.dbColumnName }

  protected[orm] lazy val defaultForeignKeyField = {
    val className = managedKlass.getName.split('.').last
    className.head.toLower + className.tail + "Id"
  }

  private lazy val fieldNames = fields.map{ _.dbColumnName }

  private var fieldsByName: immutable.HashMap[ String, MappedField ] =
    immutable.HashMap.empty

  private [orm]
  def fieldByDbName( s: String ) = {
    fieldsByName.get( s ) match {
      case Some( field ) => field
      case None =>
        fields.find { _.dbColumnName == s } match {
          case Some( field ) =>
            fieldsByName += (( s, field ))
            field
          case None =>
            throw new RuntimeException( "No field " + s + " for " + 
                                        managedKlass.getName )
        }
    }
  }

  // Dealing with the data... internals

  private
  lazy val dependencyGetterOption = 
    ReflectUtils.extractor( managedKlass,
                            classOf[ HasManyAssociation[_] ])

  protected
  def killDependentRecords( qry: ContentQuery[_,_] ) = {

    // It would be a lot more efficient to do this in the DB,
    // if we're talking to a DB, for large numbers of joins.
    // But that strategy has... certain problems when dealing
    // with a ContentProvider, unless we're privileged to know
    // a *lot* about its internals.  For small record sets,
    // which we're targeting initially, the following is
    // merely very bad and not horrid...

    dependencyGetterOption.map { getDeps =>
      for (rec <- fetchRecords( qry )) {
        for ((name, dep) <- getDeps( rec )) {
          dep.handleVanishingParent
        }
      }
    }
  }

  private [orm]
  def fetchRecords( qry: ContentQuery[_,_] ): IndexedSeq[ T ] = {
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
  def save( rec: T ) = {
    val data = nonKeyFields.map{ f => f.valPair( rec ) }
    if (rec.isNewRecord) 
      insert( data )
    else
      update( rec, data )
  }

  // The following are here to be overridden in classes that want to
  // implement soft deletion policies, versioning, audit trails, or
  // whatever.

  protected 
  def queryForRecord( rec: T ) =
    repository.whereEq( primaryKeyField.valPair ( rec ))

  protected
  def queryForAll( qry: ContentQuery[_,_] ) = qry

  protected
  def insert( vals: Seq[(String, ContentValue)] ) = repository.insert( vals:_* )

  protected
  def update( rec: T, vals: Seq[(String, ContentValue)] ) =
    queryForRecord( rec ).update( vals:_* )

  protected
  def delete( rec: T ): Unit = deleteAll( queryForRecord( rec ))

  protected
  def deleteAll( qry: ContentQuery[_,_] ): Unit = {
    killDependentRecords( queryForAll( qry ))
    queryForAll( qry ).delete
  }

  def handleVanishingParent( qry: ContentQuery[_,_] ) :Unit = qry.delete

  protected
  def updateAll( qry: ContentQuery[_,_], vals: Seq[(String,ContentValue)]):Unit=
    queryForAll( qry ).update( vals: _* )
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

      val existingField = 
        fieldsBuffer.find(mappedField => 
            mappedField.dbColumnName == dbColName
            || mappedField.recordFieldName == recordFieldName )

      existingField match {
        case Some(x) => 
          repository.log( "=== For " + managedKlass.getName +
                          " found existing mapping for " + dbColName + 
                          " db name " + x.dbColumnName + 
                          " rec name " + x.recordFieldName )
                              
        case None => {
          javaFields.get( recordFieldName ) match {
            case None => // do nothing
            case Some( javaField ) =>

              // Try to map automatically
        
              repository.log( "=== For " + managedKlass.getName +
                             " attempting to map " + dbColName + " to " +
                             recordFieldName )

              mapField( recordFieldName, dbColName, dbColName == "_id" )
          }
        }
      }
    }

    dummyCursor.close
    return super.fieldsSeq
  }

  def camelize( str: String ) = str.split("_").reduceLeft{ _ + _.capitalize }
}
