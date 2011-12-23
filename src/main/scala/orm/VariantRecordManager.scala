package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._
import org.positronicnet.util._

import android.database.Cursor

import scala.collection._

abstract class VariantRecordManager[ T <: ManagedRecord : ClassManifest ](repository: ContentQuery[_,_], discriminantColumn: String)
  extends BaseRecordManager[T]( repository )
{
  private
  var taggedVariants: Map[ String, BaseVariant[ TT ] forSome {type TT <: T}] = 
    Map.empty

  private
  var catchAllVariant: Option[BaseVariant[ TT ] forSome {type TT <: T}] = None

  private
  lazy val discriminantFieldIndex =
    allBaseCursorColumns.indexOf( discriminantColumn )

  private [orm]
  lazy val allBaseCursorColumns = collectAllColumns

  private 
  lazy val baseCursorColumnIndexByName = 
    ((0 to allBaseCursorColumns.size - 1) map { 
      (i:Int) => (allBaseCursorColumns(i), i) }).toMap

  private
  def collectAllColumns: IndexedSeq[String] = {

    var result = new immutable.HashSet[String]

    for (variant <- taggedVariants.valuesIterator)
      result = result ++ variant.fieldsSeq.map{ _.dbColumnName }

    catchAllVariant map { catchAll =>
      result = result ++ catchAll.fieldsSeq.map{ _.dbColumnName }
    }

    result += discriminantColumn

    return result.toIndexedSeq
  }
  
  override def fetchRecords( qry: ContentQuery[_,_]): IndexedSeq[T] = 
    qry.select( allBaseCursorColumns: _* ).map{ instantiateVariantFrom(_) }

  private [orm]
  def instantiateVariantFrom( c: Cursor ): T = {
    val discriminant = c.getString( discriminantFieldIndex )
    val variant = taggedVariants.get( discriminant ) orElse catchAllVariant
    variant match {
      case Some( variant ) => 
        variant.instantiateFromBaseQuery( c )
      case None => 
        throw new NoSuchVariant( this, discriminantColumn, discriminant )
    }
  }

  protected [orm]
  override def find( recId: RecordId[T], qry: ContentQuery[_,_] ) = {
    val variantMgrKludge = recId.mgr.asInstanceOf[VariantOps]
    variantMgrKludge.findAny( recId, qry )
  }

  protected [orm]
  override def save( rec: T, scope: Scope[T] ) = {
    val variantMgrKludge = rec.id.mgr.asInstanceOf[VariantOps]
    variantMgrKludge.saveAny( rec ).asInstanceOf[ RecordId[T] ]
  }

  protected [orm]
  override def delete( rec: T, scope: Scope[T] ):Unit = {
    val variantMgrKludge = rec.id.mgr.asInstanceOf[VariantOps]
    variantMgrKludge.deleteAny( rec )
  }

  // Managing the variants

  val parentRecordManager = this        // for variant RM instances.

  protected
  class BaseVariant[ TT <: T : ClassManifest ](variantQry: ContentQuery[_,_])
    extends BaseRecordManager[ TT ](variantQry)
    with VariantOps
  {
    private [orm]
    def findAny( id: RecordId[_], qry: ContentQuery[_,_] ) =
      this.find( id.asInstanceOf[ RecordId[TT] ], qry )

    private [orm]
    def saveAny( rec: ManagedRecord ): RecordId[_] =
      this.save( rec.asInstanceOf[TT], this )

    private [orm]
    def deleteAny( rec: ManagedRecord ) =
      this.delete( rec.asInstanceOf[TT], this )

    private [orm]
    lazy val fieldsFromParentQuery =
      this.fields map { field =>
        field.atIndex( baseCursorColumnIndexByName( field.dbColumnName )) }
    
    private [orm]
    def instantiateFromBaseQuery( c: Cursor ): TT =
      instantiateFrom( c, fieldsFromParentQuery )
  }

  protected
  class BaseTaggedVariant[ TT <: T : ClassManifest ]( varTag: String )
    extends BaseVariant[ TT ]( repository.whereEq(discriminantColumn -> varTag))
  {
    taggedVariants += (varTag -> this)

    protected [orm]
    override def dataPairs( rec: TT ) = 
      (super.dataPairs( rec )) :+ ( discriminantColumn -> CvString(varTag) )
  }

  protected
  class TaggedVariant[ TT <: T : ClassManifest ]( varTag: String )
    extends BaseTaggedVariant[ TT ]( varTag )
    with AutomaticFieldMappingFromQuery[ TT ]

  protected class BaseCatchAllVariant[ TT <: T : ClassManifest ]
    extends BaseVariant[ TT ]( repository )
  {
    if (catchAllVariant == None)
      catchAllVariant = Some(this)
    else
      throw new RuntimeException( "Can't have more than one catch-all for " +
                                  parentRecordManager.toString )
  }

  protected class CatchAllVariant[ TT <: T : ClassManifest ]
    extends BaseCatchAllVariant[ TT ]
    with AutomaticFieldMappingFromQuery[ TT ]

  // As of 2.9.1, it's difficult to tell the typechecker that a record of
  // any type X has a RecordManager that can be used to save that type.
  //
  // So, to fake it out, we use this hack to declare non-typechecked
  // versions of some internal operations, and then implement them with
  // casts.  (Which turn into no-ops on the JVM, due to erasure, but
  // such is life.)
  //
  // The operations as implemented are safe, because records of any given
  // type *do* have a RecordManager that can be used to save that type,
  // or things blow up in a hurry.

  protected
  trait VariantOps {
    private [orm]
    def saveAny( rec: ManagedRecord ): RecordId[_]
    private [orm]
    def deleteAny( rec: ManagedRecord ): Unit
    private [orm]
    def findAny( recId: RecordId[_], qry: ContentQuery[_,_] ): T
  }
}

class NoSuchVariant( mgr: VariantRecordManager[_], 
                     discrCol: String, 
                     discrVal: String )
  extends RuntimeException( mgr.toString + " has no variant for " +
                            discrCol + " value of " + discrVal )
