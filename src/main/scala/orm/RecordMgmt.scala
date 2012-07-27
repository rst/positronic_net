package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._
import org.positronicnet.util._
import org.positronicnet.facility._

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
  *     case class TodoItem( description: String    = null, 
  *                          isDone: Boolean        = false,
  *                          id: RecordId[TodoItem] = TodoItems.unsavedId 
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

trait ManagedRecord extends Object {

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

  val id: RecordId[_]

  /** True if this is a new record (i.e., not a query result). */

  def isNewRecord = id.isNewRecord

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

  class HasMany[T <: ManagedRecord]( src: Scope[T], foreignKey: String )
    extends HasManyAssociation( src, foreignKey, this.id )
  {
    def this( src: Scope[T] ) =
      this( src, src.mgr.columnNameFor( this.id.mgr.defaultForeignKeyField ))
  }

}

/** Representation for a record ID.
  *
  * Ordinarily just wraps a Long, but also tracks when records have been
  * saved, which can be useful when creating a base record (e.g., TodoList)
  * and several dependent records (e.g., TodoItem) in one go.
  */

class RecordId[T <: ManagedRecord] private[orm] (
    @transient val mgrArg: PrimitiveRecordManager[T],
    val id: Long)
  extends NonSharedNotifier[T]
  with Serializable
{
  def isNewRecord = id < 0

  // Bookkeeping to keep track of record manager across serialization.
  // (Doing this with custom readObject and writeObject methods
  // was... glitchy when attaching serialized RecordIds as extras to
  // Intents, so instead we do this.)
  //
  // This must be sufficient for 'mgr' (below) to reconstruct our 
  // associated BaseRecordManager.  For the base case, it *is* the
  // BaseRecordManager; for "structured repositories", things may
  // be more complex, but those will have associated ID subtypes.
  // (Invisible to clients, since the RecordManager ultimately
  // produces all IDs.)

  @transient private var mgrCache = mgrArg
  private val className: String = mgrArg.managedKlass.getName
  
  /** Record manager for this ID.  Ordinarily it's just a reference to
    * the record manager that constructed us, but if we got serialized and
    * deserialized, the 
    */

  def topLevelScope: Scope[T] = 
    mgr match {
      case tls: Scope[T] => tls
      case _ => throw new RuntimeException(
        "naked record IDs for " + mgr.getClass + " cannot fetch unassisted")
    }

  private[orm] def mgr: PrimitiveRecordManager[T] = {
    if (mgrCache == null) {
      val retrievedMgr = PrimitiveRecordManager.forClassNamed( className )
      mgrCache = retrievedMgr.asInstanceOf[ PrimitiveRecordManager[ T ]]
    }
    mgrCache
  }

  private[orm] var savedId = id
  private[orm] def markSaved( savedId: Long ) = { this.savedId = savedId }

  protected def currentValue = 
    mgr.find( this, topLevelScope.baseQuery )

  override def equals( other: Any ) =
    other match {
      case otherId: RecordId[_] =>
        otherId.id == this.id && otherId.mgr == this.mgr
      case _ =>
        false
    }

  // hashCode --- equals doesn't check savedId; nor should this
  override def hashCode = id.asInstanceOf[Int]

  override def toString = "RecordId(" + className + ", " + id.toString + ")"

  def onThread( thunk: => Unit ) = { 
    mgr.facility match {
      case w: WorkerThread => w.runOnThread{ thunk }
      case _ => thunk
    }
  }
}

object RecordId {
  implicit def toContentValue(id: RecordId[_]):ContentValue = CvLong(id.id)
}

private [orm]
object PrimitiveRecordManager {

  private [orm] def forClassNamed( name: String ): PrimitiveRecordManager[_] = {

    // We have the name of the managed class.  We need to get the
    // record manager, which we do by a somewhat convoluted path...
    // creating a dummy object, and asking *it* what it thinks its
    // manager ought to be.

    val managedKlass = Class.forName( name )
    val builder = ReflectUtils.getObjectBuilderForClass( managedKlass )
    val dummyObject = builder().asInstanceOf[ ManagedRecord ]

    dummyObject.id.mgr
  }
}

/** Base class for mapping of [[org.positronicnet.orm.ManagedRecord]]s
  * to and from persistent storage.  It is conventional to use the
  * [[org.positronicnet.orm.RecordManager]] subclass, which provides
  * additional convenience features, but the base class is available
  * for the inevitable times when the convenience features become
  * inconvenient.
  */

abstract class BaseRecordManager[ T <: ManagedRecord : ClassManifest ]( repository: ContentQuery[_,_] )
  extends PrimitiveRecordManager[T]( repository.facility )
  with Scope[T]
{
  private [orm] val mgr = this

  val baseQuery = repository
}

private [positronicnet]
abstract class PrimitiveRecordManager[T <: ManagedRecord : ClassManifest]( val facility: AppFacility )
  extends BaseNotificationManager( facility )
{
  private var nextUnsavedId = 0

  /** ID for a new unsaved object */

  def unsavedId = {
    nextUnsavedId -= 1
    new RecordId( this, nextUnsavedId )
  }

  /** Upcasting a raw Long to an ID; sometimes useful in cases where
    * serializing "baked" record Ids is really awkward, as in the extras
    * to a pending intent...
    */

  def idFromLong( rawId: Long ) = new RecordId( this, rawId )
  // Feeding the Scope machinery what it needs

  /**
    * Produce a new object (to be populated with mapped data from a query). 
    *
    * Default is to use a niladic constructor if one exists.  Failing that,
    * if there is one constructor, and defaults for all its arguments,
    * we'll use that.  
    *
    * Note that if you override this, you might want to provide a niladic
    * constructor anyway on the class itself, or strange and unuseuful things
    * will happen when attempting to serialize and deserialize our
    * [[org.positronicnet.orm.RecordId]]s, e.g., when putting them into the
    * extras of an android `Intent`.
    */

  def newRecord = builder()
  private lazy val builder = ReflectUtils.getObjectBuilder[ T ] 

  // Setting up the mapping of fields to storage columns.
  //
  // Basics here to explicitly manipulate the list within the
  // constructor.  The mapping is frozen at first use when the
  // lazy val 'fields' is computed below...

  private [orm] val managedKlass: Class[T] = 
    classManifest[T].erasure.asInstanceOf[ Class[T] ]

  private [orm] val javaFields = 
    ReflectUtils.declaredFieldsByName( managedKlass )

  protected [orm] val fieldsBuffer = new mutable.ArrayBuffer[ MappedField ]
  protected [orm] def fieldsSeq: Seq[ MappedField ] = fieldsBuffer

  private [orm] var primaryKeyField: MappedIdField = null

  private [orm] def columnNameFor( fieldName: String ) =
    columnFor( fieldName ).dbColumnName

  private [orm] def columnFor( fieldName: String ) =
    fields.find{ _.recordFieldName == fieldName } match {
      case Some( mappedField ) => mappedField
      case None =>
        throw new RuntimeException("Can't find mapping for field " + fieldName)
    }

  /** Declare that the field named `fieldName` in our
    * [[org.positronic.orm.ManagedRecord]] subclass `T`
    * is to be mapped to the persistent storage `columnName`.
    * The `how` argument indicates whether the column is to be mapped
    * read-only, read-write, or write-once, as the appropriate value from the
    * [[org.positronicnet.orm.MapAs.How]] enumeration.  If not specified, the
    * record manager's `defaultFieldMapping` will be used; that, in turn,
    * defaults to `MapAs.ReadWrite`.
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

  def mapField( fieldName: String, columnName: String, 
                how: MapAs.How = defaultMapping ) =
    mapFieldInternal( fieldName, columnName, how, false )

  /** Declare a mapping, as with `mapField`, for a field to be
    * designated as the primary key.
    */

  def primaryKey( fieldName: String, columnName: String ) =
    mapFieldInternal( fieldName, columnName, MapAs.ReadOnly, true )

  /** Declare a particular mapping type as the "default" for ordinary
    * columns (i.e., not primary keys), unless overridden in a specific
    * mapField directive.  The default for this is `MapAs.ReadWrite`.
    *
    * May be useful for, e.g., mapping Content Providers that provide
    * pseudo-tables that are mostly read-only, but with a few writable
    * columns, in which case a `MapAs.ReadOnly` default might be more
    * appropriate.
    */

  def defaultFieldMapping( how: MapAs.How ) =
    this.defaultMapping = how

  private [orm] var defaultMapping = MapAs.ReadWrite

  private[orm]
  def mapFieldInternal( fieldName: String, 
                        columnName: String,
                        how: MapAs.How = MapAs.ReadWrite,
                        primaryKey: Boolean = false ): Unit = 
  {
    val javaField = javaFields( fieldName )

    if (javaField == null) {
      throw new IllegalArgumentException( "Can't map nonexistent field " 
                                          + fieldName )
    }

    val idx = fieldsBuffer.size
    val mappedField = MappedField.create( columnName, idx, how, javaField )

    fieldsBuffer += mappedField
    
    if (primaryKey) {
      if (primaryKeyField != null) {
        throw new IllegalArgumentException( "Multiple primary key fields" )
      }
      mappedField match {
        case f: MappedIdField => 
          primaryKeyField = f
        case _ => 
          throw new IllegalArgumentException("Primary key field must be an ID")
      }
    }
  }

  // Dealing with the mappings... internals

  def dumpFieldsMapping( sfunc: String => Unit ) =
    for (f <- fields)
      sfunc( f.toString )

  private [orm] lazy val fields = fieldsSeq
  private [orm] lazy val fieldsForUpdate =
    fields.filter{ _.mappedHow == MapAs.ReadWrite }
  private [orm] lazy val fieldsForInsert =
    fields.filter{ f => 
      (f.mappedHow == MapAs.ReadWrite) || 
      (f.mappedHow == MapAs.WriteOnce) }

  private [orm] lazy val defaultForeignKeyField = {
    val className = managedKlass.getName.split('.').last
    className.head.toLower + className.tail + "Id"
  }

  protected lazy val fieldNames = fields.map{ _.dbColumnName }

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
    qry.select( fieldNames: _* ).map{ c => instantiateFrom( c, fields ) }
  }

  private [orm] 
  def instantiateFrom( c: Cursor, fields: Seq[ MappedField ] ): T = {
    val result = newRecord
    result.unsaved = false              // ... not yet altered.
    for( field <- fields ) field.setFromCursorColumn( result, c )
    return result
  }

  // The following are here to be overridden in classes that want to
  // implement soft deletion policies, versioning, audit trails, or
  // whatever.

  protected [orm]
  def queryForRecord( rec: T, scope: Scope[T] ) = {
    this.fields                         // make sure PK is set!
    scope.baseQuery.whereEq( primaryKeyField.valPair ( rec ))
  }

  protected [orm]
  def queryForAll( qry: ContentQuery[_,_] ) = qry

  protected [orm]
  def save( rec: T, scope: Scope[T] ): RecordId[T] = {
    val data = dataPairs( rec )
    if (rec.isNewRecord) {
      rec.id.markSaved( this.insert( data, scope ) )
    }
    else {
      update( rec, data, scope )
    }
      
    return rec.id.asInstanceOf[RecordId[T]]
  }

  protected
  def dataPairs( rec: T ) = {
    val fieldsToSave = if (rec.isNewRecord) fieldsForInsert else fieldsForUpdate
    fieldsToSave.map{ _.valPair( rec ) } ++ extraDataPairs( rec )
  }

  private [orm] def extraDataPairs( rec: T ): Seq[(String, ContentValue)] = 
    Seq.empty

  private
  def insert( vals: Seq[(String, ContentValue)], scope: Scope[T] ) = 
    scope.baseQuery.insert( vals:_* ).asInstanceOf[Long]

  private
  def update( rec: T, vals: Seq[(String, ContentValue)], scope: Scope[T] ) =
    queryForRecord( rec, scope ).update( vals:_* )

  protected [orm]
  def deleteAll( qry: ContentQuery[_,_], scope: Scope[T] ): Unit = {
    killDependentRecords( queryForAll( qry ))
    queryForAll( qry ).delete
  }

  protected [orm]
  def deleteAll( scope: Scope[T] ): Unit = deleteAll( scope.baseQuery, scope )

  protected [orm]
  def find( id: RecordId[T], qry: ContentQuery[_,_] ) = {
    this.fields                         // make sure PK is set!
    fetchRecords( qry.whereEq( primaryKeyField.dbColumnName -> id.savedId ))(0)
  }

  protected [orm]
  def delete( rec: T, scope: Scope[T] ):Unit = 
    deleteAll( queryForRecord( rec, scope ), scope )

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
  with AutomaticFieldMappingFromQuery[ T ]

private [orm]
trait AutomaticFieldMappingFromQuery[ T <: ManagedRecord ]
  extends BaseRecordManager[ T ]        // not "Primitive"
{
  // Take explicit mappings, and add them to mappings for like-named
  // columns available from our repository.  For that, we have to do a
  // dummy query which will (hopefully) return no rows.
  //
  // (Which, in turn, means that we can't compute the mappings in the
  // constructor; we need to wait until the DB is open.  Which may
  // help explain some of the pretzel logic by which this code gets
  // invoked...)

  override protected[orm] def fieldsSeq: Seq[ MappedField ] = {

    val dummyCursor = baseQuery.where( "2 + 2 = 5" ).selectDefaultColumns
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

              if (dbColName == "_id")
                primaryKey( recordFieldName, dbColName )
              else                                                              
                mapField( recordFieldName, dbColName )
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
  def log( s: String ) = Log.d( facility.getLogTag, s )
}

abstract class RecordManagerForFields[ TRec <: ManagedRecord : ClassManifest,
                                       TSrc : ClassManifest ]
    ( repository: ContentQuery[_,_] )
  extends BaseRecordManager[ TRec ]( repository )
  with FieldMappingFromStaticNames[ TRec ]
{
  def this() =
    this(
      PositronicContentResolver(
        ReflectUtils.getStatic[ android.net.Uri, TSrc ]( "CONTENT_URI" )))

  // Disguised arg to constructor for the FieldMappingFromStaticNames trait

  protected lazy val fieldNamesSrcMap = 
    ReflectUtils.publicStaticValues( classOf [String], 
                                     classManifest[ TSrc ].erasure )
}

private [orm]
trait FieldMappingFromStaticNames[ T <: ManagedRecord ]
  extends PrimitiveRecordManager[ T ]
{
  // Disguised argument to constructor for the trait...

  protected val fieldNamesSrcMap: Map[ String, String ]

  override protected[orm] def fieldsSeq: Seq[ MappedField ] = {

    // This runs *after* the constructor, so explicit mappings there
    // get into fieldsBuffer first...

    for ((name, field) <- javaFields) {
      if (!fieldsBuffer.exists{ _.recordFieldName == name }) {
        fieldNamesSrcMap.get( deCamelize( field.getName )).map { colName =>
          if (colName == "_id")
            primaryKey( name, colName )
          else                                                              
            mapField( name, colName )
        }
      }
    }

    return super.fieldsSeq
  }

  private def deCamelize( s: String ): String = {

    if (s == "id") return "_ID"         // special case to bridge conventions

    var lastIdx: Int = 0
    var nextIdx: Int = -1
    var chunks: Seq[String] = Seq.empty

    while (lastIdx >= 0) {
      nextIdx = s.indexWhere(_.isUpper, from = lastIdx+1)
      if (nextIdx > 0) {
        chunks = chunks :+ (s.subSequence( lastIdx, nextIdx ).toString)
      }
      else {
        chunks = chunks :+ s.substring( lastIdx )
      }
      lastIdx = nextIdx
    }

    return chunks.map{_.toUpperCase}.reduce{_+"_"+_}
  }
}
