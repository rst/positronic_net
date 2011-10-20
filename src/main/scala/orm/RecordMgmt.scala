package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._

import android.database.Cursor
import android.util.Log

import scala.collection._

/**
  * Abstract base class for objects that will be persisted by this ORM into some
  * [[org.positronicnet.content.ContentRepository]] (be it a
  * [[org.positronicnet.db.Database]], a `ContentProvider`, or whatever).
  *
  * As you can see, the infrastructure present in class instances is
  * deliberately kept very minimal.  (The usual convention is to use
  * immutable classes for these; that means that copying has to be
  * fast, and copies very lightweight.)  The main machinery here is
  * to implement certain conventions regarding `id` fields, and to
  * allow associations to be declared.  (It is ''strongly'' recommended
  * that they be declared as `lazy val`s, again to avoid overhead on
  * construction or copying unless and until the association object
  * will actually be used.)
  *
  * Most of the heavy lifting is delegated to a
  * [[org.positronicnet.orm.RecordManager]].  As discussed in the
  * [[org.positronicnet.orm]] overview, the usual pattern is to have
  * [[org.positronicnet.orm.ManagedRecord]] subclasses pass a suitable
  * [[org.positronicnet.orm.RecordManager]] singleton into the
  * [[org.positronicnet.orm.ManagedRecord]] superclass constructor,
  * like so:
  * 
  * {{{
  *     case class TodoItem( description: String = null, 
  *                          isDone: Boolean     = false,
  *                          id: Long            = ManagedRecord.unsavedId 
  *                        )
  *       extends ManagedRecord( TodoItem )
  *
  *     object TodoItems extends RecordManager[TodoItem]( TodoDb("todo_items"))
  * }}}
  *
  * It's also possible to use a [[org.positronicnet.orm.BaseRecordManager]];
  * the difference between this and the more ordinary kind of
  * [[org.positronicnet.orm.RecordManager]] is that a
  * [[org.positronicnet.orm.RecordManager]] will
  * automatically map fields based on certain naming conventions, while a
  * [[org.positonicnet.orm.BaseRecordManager]] must be configured explicitly.
  *
  * Note also that the manager MUST be a `RecordManager[ thisclass ]`, (or
  * `BaseRecordManager[ thisclass ]`), but it's
  * awfully awkward to write that constraint...
  */

abstract class ManagedRecord( private[orm] val manager: BaseRecordManager[_] ) {

  // Bookkeeping

  private [orm] var unsaved = true

  /** Persistent ID of this record.  Must default to
    * `ManagedRecord.unsavedId` in unsaved instances constructed
    * by the no-arguments (or all-defaulted-arguments constructor)
    * of an [[org.positronicnet.orm.ManagedRecord]] subclass,
    * or in the objects constructed by the `newRecord` method
    * of the [[org.positronicnet.orm.RecordManager]], if that is
    * overridden.
    */

  val id: Long

  /** True if this is a new record (i.e., not a query result). */

  def isNewRecord = (id == ManagedRecord.unsavedId)

  /** True if this record is unsaved (i.e., a new record or
    * modified query result).
    */

  def isUnsaved   = unsaved

  /** One-to-many association.  See discussion in the
    * [[org.positronicnet.orm]] overview.  Note that the name of the
    * `foreignKey` need not be supplied if it follows common conventions.
    * (If the class that owns the `HasMany` is named `ParentClass`, the
    * conventional foreign key field is the one whose Scala name is
    * `parentClassId`, in the "child" class.  That is, if a `TodoList`
    * has many `TodoItem`s, the convention is for `TodoItem` to have
    * a `todoListId`, and that's what we use.)
    *
    * Otherwise it must be the column name for the
    * [[org.positronicnet.orm.ContentRepository]], not the name of the
    * corresponding Scala record field.
    */

  class HasMany[T <: ManagedRecord]( src: RecordManager[T], foreignKey: String )
    extends HasManyAssociation( src, foreignKey, this.id )
  {
    def this( src: RecordManager[T] ) = 
      this( src, src.columnNameFor( manager.defaultForeignKeyField ))
  }

  /** Many-to-one association.  See discussion in the
    * [[org.positronicnet.orm]] overview.  Note that the
    * `foreignKey` need not be supplied if it follows common conventions.
    * (If the class the `BelongsTo` refers to is named `ParentClass`, the
    * conventional foreign key field is the one whose Scala name is
    * `parentClassId`, in the "child" class.  That is, if a `TodoItem`
    * belongs to a `TodoList`, the convention is for `TodoItem` to have
    * a `todoListId`, and that's what we use.)
    *
    * Otherwise it must be the column name for the
    * [[org.positronicnet.orm.ContentRepository]], not the name of the
    * corresponding Scala record field.
    */

  class BelongsTo[T <: ManagedRecord]( src: RecordManager[T],
                                       foreignKeyField: MappedField )
    extends BelongsToImpl( src, foreignKeyField, this )
  {
    def this( src: RecordManager[T] ) =
      this( src, manager.columnFor( src.defaultForeignKeyField ))

    def this( src: RecordManager[T], foreignKey: String ) =
      this( src, manager.columnFor( foreignKey ))
  }

  private [orm]
  class BelongsToImpl[T <: ManagedRecord]( src: RecordManager[T],
                                           foreignKeyField: MappedField,
                                           parent: ManagedRecord
                                         )
    extends BaseNotifier( src.baseQuery.facility )
    with CachingNotifier[ T ]
  {
    protected def currentValue = {
      val scope = src.whereEq( src.primaryKeyField.dbColumnName -> 
                               foreignKeyField.getValue( parent ))
      (scope.records.fetchOnThisThread)(0)
    }
  }
}

/** Companion object for the [[org.positronicnet.orm.ManagedRecord]] class. */

object ManagedRecord {

  /** ID of all unsaved [[org.positronicnet.orm.ManagedRecord]]s */

  val unsavedId = -1
}

/** Base class for mapping of [[org.positronicnet.orm.ManagedRecord]]s
  * to and from persistent storage.  It is conventional to use the
  * [[org.positronicnet.orm.RecordManager]] subclass, which provides
  * additional convenience features, but the base class is available
  * for the inevitable times when the convenience features become
  * inconvenient.
  */

abstract class BaseRecordManager[ T <: ManagedRecord : ClassManifest ]( repository: ContentQuery[_,_] )
  extends BaseNotificationManager( repository.facility )
  with Scope[T]
{
  /**
    * Produce a new object (to be populated with mapped data from a query). 
    *
    * Default is to use a niladic constructor if one exists.  Failing that,
    * if there is one constructor, and defaults for all its arguments,
    * we'll use that.  
    *
    * In other cases, RecordManagers can override.
    */

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

  protected[orm] var primaryKeyField: MappedLongField = null

  protected[orm] def columnNameFor( fieldName: String ) =
    columnFor( fieldName ).dbColumnName

  protected[orm] def columnFor( fieldName: String ) =
    fields.find{ _.recordFieldName == fieldName } match {
      case Some( mappedField ) => mappedField
      case None =>
        throw new RuntimeException("Can't find mapping for field " + fieldName)
    }

  /** Declare that the field named `fieldName` in our
    * [[org.positronic.orm.ManagedRecord]] subclass `T`
    * is to be mapped to the persistent storage `columnName`.
    * If the `primaryKey` argument is set true, this field and
    * column will be treated as the record's primary key; this
    * currently must be a `Long`.
    *
    * For [[org.positronicnet.db.Database]] mapping, when using a
    * [[org.positronicnet.orm.RecordManager]], it is rarely
    * necessary to call this explicitly; so long as you're following
    * the usual naming conventions, the ORM will figure out what's
    * going on without being told.  Those conventions are:
    *
    *  - A column named `_id` will be mapped to a field named `id`,
    *    and will be treated as the primary key.
    *  - A column with a name `like_this` will be mapped to a
    *    camel-cased field `likeThis`.
    *
    * However, a [[org.positronicnet.orm.BaseRecordManager]] will
    * not map any columns by default, and requires you to map
    * everything explicitly.
    */

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

  private var fieldsByDbName: immutable.HashMap[ String, MappedField ] =
    immutable.HashMap.empty

  private [orm]
  def fieldByDbName( s: String ) = {
    fieldsByDbName.get( s ) match {
      case Some( field ) => field
      case None =>
        fields.find { _.dbColumnName == s } match {
          case Some( field ) =>
            fieldsByDbName += (( s, field ))
            field
          case None =>
            throw new RuntimeException( "No field " + s + " for " + 
                                        managedKlass.getName )
        }
    }
  }

  private var toDbFieldMemoized: immutable.HashMap[ String, String ] =
    immutable.HashMap.empty

  private [orm]
  def toDbFieldName( s: String ) = {
    toDbFieldMemoized.get( s ) match {
      case Some( name ) => name
      case None =>
        fields.find { _.recordFieldName == s } match {
          case Some( field ) =>
            toDbFieldMemoized += (( s, field.dbColumnName ))
            field.dbColumnName
          case None =>
            toDbFieldMemoized += (( s, s ))
            s
        }
    }
  }

  // Dealing with the data... internals

  protected
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

  /** Fetch and construct the records from a
    * [[org.positronicnet.content.ContentQuery]] on this
    * record manager's [[org.positronicnet.content.ContentRepository]].
    *
    * This is largely an internal method.  It's made available mostly
    * for the sake of ORM extensions, to be implemented as traits
    * mixed into [[org.positronicnet.orm.RecordManager]] subclasses,
    * on the model of [[org.positronicnet.orm.SoftDelete]].
    */

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

  // The following are here to be overridden in classes that want to
  // implement soft deletion policies, versioning, audit trails, or
  // whatever.

  protected [orm]
  def queryForRecord( rec: T ) =
    repository.whereEq( primaryKeyField.valPair ( rec ))

  protected [orm]
  def queryForAll( qry: ContentQuery[_,_] ) = qry

  private
  def insert( vals: Seq[(String, ContentValue)] ) = repository.insert( vals:_* )

  private
  def update( rec: T, vals: Seq[(String, ContentValue)] ) =
    queryForRecord( rec ).update( vals:_* )

  protected [orm]
  def deleteAll( qry: ContentQuery[_,_], scope: Scope[T] ): Unit = {
    killDependentRecords( queryForAll( qry ))
    queryForAll( qry ).delete
  }

  protected [orm]
  def deleteAll( scope: Scope[T] ): Unit = deleteAll( scope.baseQuery, scope )

  protected [orm]
  def save( rec: T, scope: Scope[T] ): Long = {
    val data = nonKeyFields.map{ f => f.valPair( rec ) }
    if (rec.isNewRecord) 
      return insert( data ).asInstanceOf[Long]
    else {
      update( rec, data )
      return rec.id.asInstanceOf[Long]
    }
  }

  protected [orm]
  def find( id: Long, qry: ContentQuery[_,_] ) =
    fetchRecords( qry.whereEq( primaryKeyField.dbColumnName -> id ))(0)

  protected [orm]
  def delete( rec: T, scope: Scope[T] ):Unit = 
    deleteAll( queryForRecord( rec ), scope )

  /** Support for "cascading deletes", when the parent record
    * in a many-to-one association is gone.
    *
    * This is largely an internal method.  It's made available mostly
    * for the sake of ORM extensions, to be implemented as traits
    * mixed into [[org.positronicnet.orm.RecordManager]] subclasses,
    * on the model of [[org.positronicnet.orm.SoftDelete]].
    */

  def handleVanishingParent( qry: ContentQuery[_,_] ) :Unit = qry.delete

  protected [orm]
  def updateAll( scope: Scope[T], vals: Seq[(String,ContentValue)]):Unit=
    queryForAll( scope.baseQuery ).update( vals: _* )
}

/** Class for mapping of [[org.positronicnet.orm.ManagedRecord]]s
  * to and from persistent storage.  Extends
  * [[org.positronicnet.orm.BaseRecordManager]] by providing
  * additional convenience features; most notably, it will automatically
  * map field names to corresponding columns based on naming conventions
  * without needing explicit configuration.
  *
  * See `mapField` below or the discussion of field mapping in the
  * [[org.positronicnet.orm]] overview.
  */
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
          log( "=== For " + managedKlass.getName +
               " found existing mapping for " + dbColName + 
               " db name " + x.dbColumnName + 
               " rec name " + x.recordFieldName )
                              
        case None => {
          javaFields.get( recordFieldName ) match {
            case None => // do nothing
            case Some( javaField ) =>

              // Try to map automatically
        
              log( "=== For " + managedKlass.getName +
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

  private
  def camelize( str: String ) = str.split("_").reduceLeft{ _ + _.capitalize }

  private 
  def log( s: String ) = Log.d( repository.facility.getLogTag, s )
}
