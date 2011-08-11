package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.util._
import org.positronicnet.util.AppFacility

trait BaseScope[ T <: ManagedRecord ]
  extends ChangeManager
{
  private [orm] val facility: AppFacility
  private [orm] val mgr: BaseRecordManager[T]
  private [orm] val baseQuery: ContentQuery[_,_]

  def all: BaseScope[T] = this

  def records = valueStream{ mgr.rawQuery( baseQuery ) }
  def count = baseQuery match {
    case query: { def count: Long } => valueStream{ query.count }
    case _ => throw new IllegalArgumentException( "Can't count rows from a " 
                                                  + baseQuery.getClass.getName )
  }

  def where( str: String, arr: Array[ContentValue] = null ): Scope[T] = 
    new Scope( this, baseQuery.where( str, arr ))

  def whereEq( pairs: (String, ContentValue)* ): Scope[T] =
    new Scope( this, baseQuery.whereEq( pairs: _* ))

  def deleteAll = doChange { deleteAllOnThisThread }
  def updateAll( assigns: (String, ContentValue)* ) =
    doChange { updateAllOnThisThread( assigns: _* ) }

  def countOnThisThread = baseQuery match {
    case query: { def count: Long } => query.count
    case _ => throw new IllegalArgumentException( "Can't count rows from a " 
                                                  + baseQuery.getClass.getName )
  }

  def queryOnThisThread = mgr.rawQuery( baseQuery )
  def deleteAllOnThisThread = baseQuery.delete
  def updateAllOnThisThread( assigns: (String, ContentValue)* ) = 
    baseQuery.update( assigns: _* )
}

class Scope[ T <: ManagedRecord ]( base: BaseScope[T], query: ContentQuery[_,_])
  extends ChangeManager( base.facility )
  with BaseScope[T]
{
  // Copy-constructor for the convenience of specialized subclasses...

  def this( other: Scope[T] ) = this( other.baseScope, other.baseQuery )

  private [orm] val facility  = base.facility
  private [orm] val mgr       = base.mgr
  private [orm] val baseScope = base
  private [orm] val baseQuery = query

  override def noteChange = {
    base.noteChange
    super.noteChange
  }
}
