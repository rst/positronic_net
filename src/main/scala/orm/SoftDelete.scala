package org.positronicnet.orm

import org.positronicnet.content.ContentQuery

// Our sample apps are using a "soft deletion" scheme pretty broadly.
// With this scheme, items that the user deletes aren't *really* deleted
// immediately.  Instead, they get marked with an "is_deleted" flag, which
// puts them in limbo from which they can be recalled with an "undelete".
//
// Whenever we're doing a soft deletion, similar items that already were
// in limbo do in fact go away.  
//
// To activate the basic machinery for a record type, make sure the table
// corresponding to the records has an "is_deleted" column (you don't have
// to map it), and mix SoftDelete[T] into the record manager.  
//
// This gets you something pretty aggressive about expunging prior
// "is_deleted" records --- anything in the table that already was
// "is_deleted" gets the axe.  To control that, you can mix 
// SoftDeleteScope into derived scopes (e.g. a HasMany).
//
// Also, if you have a many-to-one relation, and the parent records
// are subject to soft deletion, the child records might want to know
// about it.  You can use the ParentSoftDeleteListener trait to look
// for that...

trait ParentSoftDeleteListener[T <: ManagedRecord] {
  def onParentSoftDelete( qry: ContentQuery[_,_], scope: Scope[T] ): Unit
  def onParentUndelete( qry: ContentQuery[_,_], scope: Scope[T] ): Unit
}

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

  private[orm] lazy val parentSoftDeleteListenerOption =
    dependencyGetterOption.map { getDeps =>
      getDeps( newRecord ).values.map{ _.mgr }.filter{ mgr => 
        mgr.isInstanceOf[ ParentSoftDeleteListener[T] ] }}

  protected override def deleteAll( qry: ContentQuery[_,_], scope: Scope[T] ): Unit = {
    val enclosingScope = findSoftDeleteScope( scope )
    val enclosingScopeQry = super.queryForAll( enclosingScope.baseQuery )
    enclosingScopeQry.whereEq( "is_deleted" -> true ).delete

    parentSoftDeleteListenerOption.map { parentListeners =>
      parentListeners.foreach{ listener =>
        listener.asInstanceOf[ ParentSoftDeleteListener[T] ].onParentSoftDelete( qry, scope ) }}

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

object SoftDeleteActions {
  def Undelete[T <: ManagedRecord : ClassManifest ] = UndeleteAction[T](0)
}

case class UndeleteAction[T <: ManagedRecord : ClassManifest ](dummy: Int) 
  extends ScopedAction[T]
{
  def act( scope: Scope[T], mgr: BaseRecordManager[T] ): Unit = {

    mgr match {
      case sd: SoftDelete[T] =>
        val undeletingItems = scope.baseQuery.whereEq( "is_deleted" -> true )
        sd.parentSoftDeleteListenerOption.map { parentListeners =>
          parentListeners.foreach{ listener =>
            listener.asInstanceOf[ ParentSoftDeleteListener[T] ].onParentUndelete( undeletingItems, scope ) }}
      case _ =>
        throw new RuntimeException( mgr.toString + " does not do soft delete" )
    }

    scope.baseQuery.whereEq("is_deleted" -> true).update("is_deleted" -> false)
  }
}

