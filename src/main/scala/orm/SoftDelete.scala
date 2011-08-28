package org.positronicnet.orm

import org.positronicnet.content.ContentQuery

// We're using a "soft deletion" scheme pretty broadly, which plugs in
// like so.  (Note that the "is_deleted" column in a soft-delete table
// doesn't need to be mapped in core, and usually isn't.)

trait SoftDeleteQueries[ T <: ManagedRecord ] extends Scope[ T ]
{
  def numDeleted = valueStream{ baseQuery.whereEq("is_deleted"->true).count }
  def hasDeleted = valueStream{ baseQuery.whereEq("is_deleted"->true).count > 0}
}

trait SoftDeleteScope[ T <: ManagedRecord ] extends SoftDeleteQueries[ T ]

trait SoftDelete[ T <: ManagedRecord ]
  extends BaseRecordManager[ T ]
  with SoftDeleteScope[ T ]
{
  protected override def queryForAll( qry: ContentQuery[_,_] ) =
    super.queryForAll( qry ).whereEq( "is_deleted" -> false )

  protected override def deleteAll( qry: ContentQuery[_,_], scope: Scope[T] ): Unit = {
    val enclosingScope = findSoftDeleteScope( scope )
    val enclosingScopeQry = super.queryForAll( enclosingScope.baseQuery )
    enclosingScopeQry.whereEq( "is_deleted" -> true ).delete
    super.queryForAll( qry ).update( "is_deleted" -> true )
  }

  def findSoftDeleteScope( scope: Scope[T] ): SoftDeleteScope[T] =
    scope match {
      case gotIt: SoftDeleteScope[T] => gotIt
      case derived: DerivedScope[T]  => findSoftDeleteScope( derived.baseScope )
      case _ =>
        // "Can't happen"; root of the hierarchy is the record mgr itself,
        // which is a soft delete scope...
        throw new RuntimeException("Can't find enclosing scope for soft delete")
    }
}

case class Undelete[T <: ManagedRecord : ClassManifest ](dummy: T) 
  extends ScopedAction[T]
{
  def act( scope: Scope[T], mgr: BaseRecordManager[T] ): Unit =
    scope.baseQuery.whereEq("is_deleted" -> true).update("is_deleted" -> false)
}

