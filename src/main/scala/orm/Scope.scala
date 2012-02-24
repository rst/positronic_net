package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._
import org.positronicnet.facility.AppFacility

import scala.collection.mutable.HashMap

abstract class ScopeAction[T] extends Action[IndexedSeq[T]]
abstract class ScopeQueryAction[T,V] extends QueryAction[IndexedSeq[T],V]

/** Actions that can be sent to [[org.positronicnet.orm.RecordManager]]s
  * and other [[org.positronicnet.orm.Scope]]s.
  * 
  * Typical syntax is `scope ! action` (to have the action performed
  * on a background thread) or `scope.onThisThread(action)` (to have
  * the action performed on the current thread).
  *
  * Note that any Scope (including a RecordManager) will also act as
  * a `Notifier[IndexedSeq[T]]` (where `T` is the record type), and
  * as such, can receive [[org.positronicnet.notification.Actions]]
  * as well; this is where you can find `Fetch`, `AddWatcher` and
  * friends (as in `RecordManager!Fetch{...}`).
  */

object Actions {

  private [orm] case class FindAction[T <: ManagedRecord]( id: RecordId[T], 
                                                           handler: T => Unit) 
                     extends ScopeAction[T]
  private [orm] case class DeleteAllAction[T]( dummy: Long = 0 ) 
                     extends ScopeAction[T]

  /** Request to save an existing record (an update), or a new
    * one (an insert).
    */

  case class Save[T]( record: T ) extends ScopeAction[T]

  /** Request to delete an existing record. */

  case class Delete[T]( record: T ) extends ScopeAction[T]

  /** Request to update all the records matching this scope.
    * The argument is a set of
    * `(String,[[org.positronicnet.content.ContentValue]])` pairs,
    * often entered using `->` notation, like so:
    * {{{
    *     joesBugs ! UpdateAll( "status"->"done", "updated_at"->now )
    * }}}
    * The strings are currently the names of columns in the underlying
    * resource, and not the names of fields they get mapped to.
    * (XXX This should be fixed.)
    */

  case class UpdateAll[T]( vals: (String, ContentValue)* )
    extends ScopeAction[T]

  // At least in Scala 2.8.1, case objects can't take type parameters.
  // So, for DeleteAll we have this disreputable-looking hack...

  /** Request to delete all of the records matching this scope. */

  def DeleteAll[T] = DeleteAllAction[T](0)

  /** Find the record with the given `id`, if it exists.
    * 
    * XXX should only find it if it matches conditions of the
    * scope, as per Rails.
    */

  def Find[T <: ManagedRecord]( id: RecordId[T] )( handler: T => Unit ) =
    FindAction( id, handler )


  /** QueryAction yielding a future for the record with the given `id`,
    * if it exists.
    * 
    * XXX should only find it if it matches conditions of the
    * scope, as per Rails.
    */

  case class FindById[T <: ManagedRecord]( id: RecordId[T] )
    extends ScopeQueryAction[T,T]
  {
    val complete: PartialFunction[ BaseNotifierImpl[ IndexedSeq[T] ], T ] = {
      case dummy: BaseNotifierImpl[ IndexedSeq[T] ] =>
        id.mgr.find( id, id.mgr.baseQuery )
    }
  }
    
}

import Actions._

/** Utility class for declare new Scope-specific actions, to do whatever
  * is defined in the corresponding `act` method.
  * 
  * Useful mainly if you're defining one of your own (e.g., to
  * support an ORM extension like [[org.positronicnet.orm.SoftDelete]]).
  */

abstract class ScopedAction[T <: ManagedRecord: ClassManifest] 
  extends ScopeAction[T]
{
  /** The method that actually does the work of handling the
    * action, whatever that entails.  Arguments are the
    * [[org.positronicnet.orm.Scope]] that receieved the action,
    * and, for convenience, its associated
    * [[org.positronicnet.orm.RecordManager]].
    *
    * The method is invoked on the `RecordManager`'s associated
    * worker thread if the action was sent as `scope!action`,
    * and on the calling thread if it was sent instead as
    * `scope.onThisThread(action)`.
    */

  def act( scope: Scope[T], mgr: BaseRecordManager[T] ): Unit
}

/** A [[org.positronicnet.orm.Scope]] represents a subset of the
  * [[org.positronicnet.orm.ManagedRecord]]s managed by some
  * [[org.positronicnet.orm.RecordManager]], which may be used
  * to query and update them.
  *
  * (These are the same methods available by using the
  * [[org.positronicnet.orm.RecordManager]] directly, which is
  * no coincidence --- [[org.positronicnet.orm.RecordManager]]
  * extends [[org.positronicnet.orm.Scope]].)
  */

trait Scope[ T <: ManagedRecord ]
  extends NotificationManager
  with NotifierDelegator[ IndexedSeq[ T ]]
{
  private [orm] val mgr: BaseRecordManager[T]

  /** [[org.positronicnet.facility.AppFacility]] associated with the
    * content source.
    */

  val facility: AppFacility

  /** [[org.positronicnet.content.ContentQuery]] whose conditions
    * match those declared for the [[org.positronicnet.orm.Scope]].
    */

  val baseQuery: ContentQuery[_,_]

  /** [[org.positronicnet.content.ContentQuery]] whose conditions
    * match those for the records in scope.  Ordinarily the same
    * as `baseQuery`, but some [[org.positronicnet.orm.RecordManager]]s
    * may add extra conditions to, e.g., make soft-deleted records
    * invisible from an ordinary fetch.
    */

  lazy val fullQuery = mgr.queryForAll( baseQuery )

  // XXX these should be library-private:

  lazy val records = valueNotifier{ mgr.fetchRecords( fullQuery )}
  def notifierDelegate = records        // for NotifierDelegator

  /** [[org.positronicnet.notifications.Notifier]] which can be used
    * to fetch, or to receive updates on, the number of records matching
    * the scope's conditions.  One usage pattern:
    * {{{
    *     Bugs.count ! AddWatcher( this ) { bugCount =>
    *       if (bugCount == 0) {
    *         startActivity( new Intent( this, classOf[ CelebrationActivity ])
    *         Bugs ! StopWatcher( this )
    *         finish
    *       }
    *     }
    * }}}
    * The `onChangeTo` method in
    * [[org.positronicnet.ui.PositronicActivityHelpers]] can simplify this
    * a bit... but I digress.
    *
    * Note that `count` is supported only if it is supported by the underlying
    * [[org.positronicnet.content.ContentRepository]]; e.g., by databases, but
    * not by ContentProviders.
    */

  lazy val count = valueNotifier{ fullQuery.count }

  /** Returns a notifier for a subset of the records covered by this
    * scope, matching extra conditions, which can be changed later by sending
    * a `Requery` message.
    *
    * This can be used, for example, as the source for an
    * [[org.positronicnet.ui.IndexedSeqSourceAdapter]], which displays
    * the rows matching the user's "current search condition", whatever
    * it may be, and changes when the condition changes, but ''without''
    * having to rewire the [[org.positronicnet.ui.IndexedSeqSourceAdapter]],
    * perhaps something like so:
    * {{{
    *     val matchingBugs =
    *       Bugs.recordsQuery( "" ){ pattern =>
    *         Bugs.where( "tag_line like ?", pattern )
    *       }
    *
    *      // ... later ...
    *
    *     matchingBugs ! Requery( "newpattern" )
    * }}}
    * The `Requery` will cause any declared watchers of `matchingBugs`
    * to be notified of the new query result.
    */

  def recordsQuery[ Q ]( initial: Q )( fn: Q => Scope[T] ) = 
    valueQuery( initial ){ q => 
      mgr.fetchRecords( mgr.queryForAll( fn(q).baseQuery )) }

  /** As for `recordsQuery`, but returns a notifier of the count of
    * records matching the extra conditions.
    */

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

  /** Produce a restricted subscope containing only records
    * matching an extra SQL condition, possibly including
    * `?` placeholders which can get filled in by the supplied
    * [[org.positronicnet.content.ContentValue]] arguments, if
    * any.  Example:
    * {{{
    *     val recentBugs =
    *       Bugs.where( "state != ? and create_date > ?",
    *                   "closed",
    *                   System.currentTimeMillis - 3*86400*1000 )
    * }}}
    * Note that the columns are named using internal SQL column
    * names, not the Java mapped versions.
    *
    * (Note ''for the future'':  we could consider changing this by
    * allowing syntax like `"{createDate}>?"`, where the string
    * in curly-braces would be a Java field name to be mapped to
    * the SQL equivalent.  But that's not there yet.)
    */

  def where( str: String, vals: ContentValue* ): Scope[T] = 
    subScopeFor( baseQuery.where( str, vals: _* ))

  /** Produce a restricted subscope containing only records
    * where particular fields have particular values.
    * The argument is a set of
    * `(String,[[org.positronicnet.content.ContentValue]])` pairs,
    * often entered using `->` notation, like so:
    * {{{
    *     Bugs.whereEq( "status"->"done", "ownerId"->joe.id )
    * }}}
    * Note that columns can be designated by either the names
    * of the columns in the underlying tables, or the names of the
    * Scala `val`s in the classes that they get mapped to.
    */

  def whereEq( pairs: (String, ContentValue)* ): Scope[T] = {
    val ppairs = pairs.map{ (pair) =>
      ( mgr.toDbFieldName( pair._1 ), pair._2 ) }
    subScopeFor( baseQuery.whereEq( ppairs: _* ))
  }

  /** Produce a modified subscope which supplies records in a particular
    * order.  The string supplied is used directly in a SQL `order`
    * clause (or passed to the underlying `ContentProvider`, if
    * appropriate).
    */

  def order( str: String ): Scope[T] = 
    new AlternateViewScope( this, baseQuery.order( str ))

  /** Produce a modified subscope which supplies at most a given
    * number of records.  The string supplied is used directly in a SQL `limit`
    * clause.
    * May not be supported by all repositories; in particular, it is
    * not supported by ContentProviders.
    */

  def limit( str: String ): Scope[T] =
    new AlternateViewScope( this, baseQuery.limit( str ))

  /** Produce a modified subscope which supplies at most a given
    * number of records.  The string supplied is used directly in a SQL `limit`
    * clause.
    * May not be supported by all repositories; in particular, it is
    * not supported by ContentProviders.
    */

  def limit( lim: Int ): Scope[T] =
    new AlternateViewScope( this, baseQuery.limit( lim ))

  /** Find the record with the given `id`, if it exists, running the
    * query on the current thread, and returning it as the value.
    * 
    * XXX should only find it if it matches conditions of the
    * scope, as per Rails.
    */

  def findOnThisThread( id: RecordId[T] ) = mgr.find( id, baseQuery )

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

  def ?[V]( action: QueryAction[ IndexedSeq[T], V ]) : Future[V] =
    records ? action

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

private[orm]
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

private [orm]
class HasManyAssociation[ T <: ManagedRecord ]( base:       Scope[ T ],
                                                foreignKey: String, 
                                                idVal:      RecordId[_]
                                              )
  extends AlternateViewScope( base.whereEq( foreignKey -> idVal.id ))
{
  override def toString = "HasMany: " + notificationManagerDelegate.toString

  lazy val foreignKeyField = mgr.fieldByDbName( foreignKey ).recordField

  /** Create a new child record, with the foreign key field pre-populated.
    */

  def create: T = {
    val target = mgr.newRecord
    foreignKeyField.set( target, idVal )
    target
  }

  /** The code invoked when the "parent" in a `HasMany` vanishes, to
    * mop up child records.
    */

  def handleVanishingParent: Unit = {
    mgr.handleVanishingParent( baseQuery )
    noteChange
  }
}
