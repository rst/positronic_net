package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.util._
import org.positronicnet.util.AppFacility

import scala.collection.mutable.HashMap

abstract class ScopeAction[T] extends Action[IndexedSeq[T]]

case class Save[T]( record: T ) extends ScopeAction[T]
case class Delete[T]( record: T ) extends ScopeAction[T]
case class DeleteAll[T]( dummy: T ) extends ScopeAction[T]
case class UpdateAll[T]( vals: (String, ContentValue)* )
  extends ScopeAction[T]

abstract class ScopedAction[T <: ManagedRecord: ClassManifest] 
  extends ScopeAction[T]
{
  def act( scope: Scope[T], mgr: BaseRecordManager[T] ): Unit
}

trait Scope[ T <: ManagedRecord ]
  extends NotificationManager
{
  private [orm] val mgr: BaseRecordManager[T]

  val facility: AppFacility
  val baseQuery: ContentQuery[_,_]

  lazy val fullQuery = mgr.queryForAll( baseQuery )

  lazy val records = valueStream{ mgr.fetchRecords( fullQuery )}

  lazy val count = valueStream{ fullQuery.count }

  def recordsQuery[ Q ]( initial: Q ) 
                       ( fn: (Q, ContentQuery[_,_]) => ContentQuery[_,_] ) = 
    valueQuery( initial ){ q => 
      mgr.fetchRecords( mgr.queryForAll( fn ( q, baseQuery ))) }

  def countQuery[ Q ]( initial: Q ) 
                     ( fn: (Q, ContentQuery[_,_]) => ContentQuery[_,_] ) = 
    valueQuery( initial ){ q => mgr.queryForAll( fn ( q, baseQuery )).count }

  // Want to be sure we have only *one* sub-scope for any set of
  // conditions, so notifications get properly shared.  Thus the
  // following.  (NB that subScopes is a potential storage leak;
  // if that gets to be a problem, we'd have to make the values
  // weakRefs, and periodically sweep the table looking for ones
  // that have snapped.)

  private var subScopes: HashMap[( String, Seq[ String ]), Scope[T]] =
    HashMap.empty

  protected[orm]
  def subScopeFor( query: ContentQuery[_,_] ): Scope[T] =
    this.synchronized {
      subScopes.get( query.conditionKey ) match {
        case Some( scope ) => 
          scope
        case None =>
          val newScope = new SubScope( this, query )
          subScopes( query.conditionKey ) = newScope
          newScope
      }
    }

  // Conditions and other derivatives

  def where( str: String, arr: Array[ContentValue] = null ): Scope[T] = 
    subScopeFor( baseQuery.where( str, arr ))

  def whereEq( pairs: (String, ContentValue)* ): Scope[T] =
    subScopeFor( baseQuery.whereEq( pairs: _* ))

  def order( str: String ): Scope[T] = 
    new AlternateViewScope( this, baseQuery.order( str ))

  def limit( str: String ): Scope[T] =
    new AlternateViewScope( this, baseQuery.limit( str ))

  def limit( lim: Int ): Scope[T] =
    new AlternateViewScope( this, baseQuery.limit( lim ))

  // Action interface.

  def !( action: Action[ IndexedSeq[T]] ): Unit = action match {
    case a: NotifierAction[ IndexedSeq [T] ] => records ! a
    case _ => onThread{ onThisThread( action ) }
  }

  def fetchOnThisThread = records.fetchOnThisThread

  def onThisThread( action: Action[ IndexedSeq [T]] ): Unit = action match {

    case a: NotifierAction[ IndexedSeq [T] ] => records.onThisThread( a )

    case Save( record )     => mgr.save( record, this );      noteChange
    case Delete( record )   => mgr.delete( record, this );    noteChange
    case DeleteAll( dummy ) => mgr.deleteAll( this );         noteChange
    case u: UpdateAll[ T ]  => mgr.updateAll( this, u.vals ); noteChange

    case a: ScopedAction[T] => a.act( this, mgr );            noteChange

    case _ => 
      throw new IllegalArgumentException( "Unrecognized action: " + 
                                          action.toString )
  }
}

private[orm]
class SubScope[ T <: ManagedRecord ]( base: Scope[T], 
                                      query: ContentQuery[_,_])
  extends BaseNotificationManager( base.facility )
  with Scope[T]
{
  private [orm] val mgr       = base.mgr

  val facility  = base.facility
  val baseQuery = query

  override def toString = {
    val (str, vals) = query.conditionKey
    val valsStr = vals.reduceLeft{ _.toString + _.toString }
    base.toString + " where [" + str + "; " + valsStr + "]" 
  }

  override def noteChange = {
    super.noteChange
    base.noteChange
  }
}

private[orm]
class AlternateViewScope[ T <: ManagedRecord ]( base: Scope[T],
                                                query: ContentQuery[_,_]
                                              )
  extends BaseNotificationDelegator( base )
  with Scope[ T ]
{
  def this( base: Scope[T] ) = this( base, base.baseQuery )

  private [orm] val mgr = delegate.mgr

  val facility  = delegate.facility
  val baseQuery = query
}

class HasManyAssociation[ T <: ManagedRecord ]( base:       Scope[ T ],
                                                foreignKey: String, 
                                                idVal:      ContentValue
                                              )
  extends AlternateViewScope( base.whereEq( foreignKey -> idVal ))
{
  override def toString = "HasMany: " + delegate.toString

  lazy val foreignKeyField = mgr.fieldByDbName( foreignKey )

  def create: T = {
    val target = mgr.newRecord
    foreignKeyField.setValue( target, idVal )
    target
  }

  def handleVanishingParent: Unit = {
    mgr.handleVanishingParent( baseQuery )
    noteChange
  }
}
