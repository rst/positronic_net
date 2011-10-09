package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._
import org.positronicnet.facility.AppFacility

import scala.collection.mutable.HashMap

abstract class ScopeAction[T] extends Action[IndexedSeq[T]]

object Actions {

  private [orm] case class FindAction[T]( id: Long, handler: T => Unit) 
                     extends ScopeAction[T]
  private [orm] case class DeleteAllAction[T]( dummy: Long = 0 ) 
                     extends ScopeAction[T]

  case class Save[T]( record: T ) extends ScopeAction[T]
  case class Delete[T]( record: T ) extends ScopeAction[T]
  case class UpdateAll[T]( vals: (String, ContentValue)* )
    extends ScopeAction[T]

  // At least in Scala 2.8.1, case objects can't take type parameters.
  // So, for DeleteAll we have this disreputable-looking hack...

  def DeleteAll[T] = DeleteAllAction[T](0)
  def Find[T]( id: Long )( handler: T => Unit ) =
    FindAction( id, handler )
}

import Actions._

abstract class ScopedAction[T <: ManagedRecord: ClassManifest] 
  extends ScopeAction[T]
{
  def act( scope: Scope[T], mgr: BaseRecordManager[T] ): Unit
}

trait Scope[ T <: ManagedRecord ]
  extends NotificationManager
  with NotifierDelegator[ IndexedSeq[ T ]]
{
  private [orm] val mgr: BaseRecordManager[T]

  val facility: AppFacility
  val baseQuery: ContentQuery[_,_]

  lazy val fullQuery = mgr.queryForAll( baseQuery )

  lazy val records = valueNotifier{ mgr.fetchRecords( fullQuery )}
  def notifierDelegate = records        // for NotifierDelegator

  lazy val count = valueNotifier{ fullQuery.count }

  def recordsQuery[ Q ]( initial: Q )( fn: Q => Scope[T] ) = 
    valueQuery( initial ){ q => 
      mgr.fetchRecords( mgr.queryForAll( fn(q).baseQuery )) }

  def countQuery[ Q ]( initial: Q )( fn: Q => Scope[T] ) = 
    valueQuery( initial ){ q => mgr.queryForAll( fn(q).baseQuery ).count }

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

  def where( str: String, vals: ContentValue* ): Scope[T] = 
    subScopeFor( baseQuery.where( str, vals: _* ))

  def whereEq( pairs: (String, ContentValue)* ): Scope[T] = {
    val ppairs = pairs.map{ (pair) =>
      ( mgr.toDbFieldName( pair._1 ), pair._2 ) }
    subScopeFor( baseQuery.whereEq( ppairs: _* ))
  }

  def order( str: String ): Scope[T] = 
    new AlternateViewScope( this, baseQuery.order( str ))

  def limit( str: String ): Scope[T] =
    new AlternateViewScope( this, baseQuery.limit( str ))

  def limit( lim: Int ): Scope[T] =
    new AlternateViewScope( this, baseQuery.limit( lim ))

  // 'find' support...

  def findOnThisThread( id: Long ) = mgr.find( id, baseQuery )

  // Action interface.

  override def !( action: Action[ IndexedSeq[T]] ): Unit = action match {
    case a: NotifierAction[ IndexedSeq [T] ] => 
      records ! a
    case FindAction( id, handler ) => 
      val wrapped = CallbackManager.wrapHandler( handler )
      onThread{ wrapped( mgr.find( id, baseQuery )) }
    case _ => 
      onThread{ onThisThread( action ) }
  }

  override def onThisThread( action: Action[ IndexedSeq [T]] ) = action match {

    case a: NotifierAction[ IndexedSeq [T] ] => records.onThisThread( a )

    case FindAction( id, handler ) => handler( mgr.find( id, baseQuery ))
    case Save( record )            => mgr.save( record, this );      noteChange
    case Delete( record )          => mgr.delete( record, this );    noteChange
    case DeleteAllAction( x )      => mgr.deleteAll( this );         noteChange
    case u: UpdateAll[ T ]         => mgr.updateAll( this, u.vals ); noteChange

    case a: ScopedAction[T]        => a.act( this, mgr );            noteChange

    case _ => 
      throw new IllegalArgumentException( "Unrecognized action: " + 
                                          action.toString )
  }
}

trait DerivedScope[ T <: ManagedRecord ] 
  extends Scope[ T ]
{
  val baseScope: Scope[T]
}

private[orm]
class SubScope[ T <: ManagedRecord ]( base: Scope[T], 
                                      query: ContentQuery[_,_])
  extends BaseNotificationManager( base.facility )
  with DerivedScope[T]
{
  private [orm] val mgr       = base.mgr

  val facility  = base.facility
  val baseQuery = query
  val baseScope = base

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
  with DerivedScope[ T ]
{
  def this( base: Scope[T] ) = this( base, base.baseQuery )

  private [orm] val mgr = notificationManagerDelegate.mgr

  val facility  = notificationManagerDelegate.facility
  val baseQuery = query
  val baseScope = base
}

class HasManyAssociation[ T <: ManagedRecord ]( base:       Scope[ T ],
                                                foreignKey: String, 
                                                idVal:      ContentValue
                                              )
  extends AlternateViewScope( base.whereEq( foreignKey -> idVal ))
{
  override def toString = "HasMany: " + notificationManagerDelegate.toString

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
