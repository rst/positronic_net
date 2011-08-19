package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.util._
import org.positronicnet.util.AppFacility

abstract class ScopeAction[T] extends Action[IndexedSeq[T]]

case class Save[T]( record: T ) extends ScopeAction[T]
case class Delete[T]( record: T ) extends ScopeAction[T]
case class DeleteAll[T]( dummy: T ) extends ScopeAction[T]
case class UpdateAll[T]( vals: (String, ContentValue)* )
  extends ScopeAction[T]

trait BaseScope[ T <: ManagedRecord ]
  extends ChangeManager
{
  private [orm] val facility: AppFacility
  private [orm] val mgr: BaseRecordManager[T]
  private [orm] val baseQuery: ContentQuery[_,_]

  lazy val fullQuery = mgr.queryForAll( baseQuery )

  lazy val records = valueStream{ mgr.fetchRecords( fullQuery )}

  lazy val count = 
    fullQuery match {

      // Warnings on the next line --- always matches due to type erasure.
      // So, the catch-all below it catches nothing.
      // Sigh...

      case query: { def count: Long } => valueStream{ query.count }
      case _ => throw new IllegalArgumentException( "Can't count rows from a " 
                                                   + fullQuery.getClass.getName)
    }

  // Conditions

  def where( str: String, arr: Array[ContentValue] = null ): Scope[T] = 
    new Scope( this, baseQuery.where( str, arr ))

  def whereEq( pairs: (String, ContentValue)* ): Scope[T] =
    new Scope( this, baseQuery.whereEq( pairs: _* ))

  // Action interface.

  def !( action: Action[ IndexedSeq[T]] ): Unit = action match {
    case a: NotifierAction[ IndexedSeq [T] ] => records ! a
    case _ => onThread{ onThisThread( action ) }
  }

  def fetchOnThisThread = records.fetchOnThisThread

  def onThisThread( action: Action[ IndexedSeq [T]] ): Unit = action match {

    case a: NotifierAction[ IndexedSeq [T] ] => records.onThisThread( a )

    case Save( record )     => mgr.save( record );                 noteChange
    case Delete( record )   => mgr.delete( record );               noteChange
    case DeleteAll( dummy ) => mgr.deleteAll( baseQuery );         noteChange
    case u: UpdateAll[ T ]  => mgr.updateAll( baseQuery, u.vals ); noteChange

    case _ => 
      throw new IllegalArgumentException( "Unrecognized action: " + 
                                          action.toString )
  }
}

class Scope[ T <: ManagedRecord ]( base: BaseScope[T], query: ContentQuery[_,_])
  extends ChangeManager( base.facility )
  with BaseScope[T]
{
  private [orm] val facility  = base.facility
  private [orm] val mgr       = base.mgr
  private [orm] val baseScope = base
  private [orm] val baseQuery = query

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
