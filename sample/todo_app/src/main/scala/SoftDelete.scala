package org.positronicnet.sample.todo

import org.positronicnet.orm._
import org.positronicnet.content.ContentQuery

// We're using a "soft deletion" scheme pretty broadly, which plugs in
// like so.  (Note that the "is_deleted" column in a soft-delete table
// doesn't need to be mapped in core, and usually isn't.)

trait SoftDeleteQueries[ T <: ManagedRecord ] extends Scope[ T ]
{
  def numDeleted = valueStream{ baseQuery.whereEq("is_deleted"->true).count }
  def hasDeleted = valueStream{ baseQuery.whereEq("is_deleted"->true).count > 0}
}

trait SoftDelete[ T <: ManagedRecord ]
  extends BaseRecordManager[ T ]
  with SoftDeleteQueries[ T ]
{
  protected override def queryForAll( qry: ContentQuery[_,_] ) =
    super.queryForAll( qry ).whereEq( "is_deleted" -> false )

  protected override def deleteAll( qry: ContentQuery[_,_] ): Unit = {
    super.queryForAll( qry ).whereEq( "is_deleted" -> true ).delete
    super.queryForAll( qry ).update( "is_deleted" -> true )
  }
}

case class Undelete[T <: ManagedRecord : ClassManifest ](dummy: T) 
  extends ScopedAction[T]
{
  def act( qry: ContentQuery[_,_], mgr: BaseRecordManager[T] ): Unit =
    qry.whereEq( "is_deleted" -> true ).update( "is_deleted" -> false )
}

