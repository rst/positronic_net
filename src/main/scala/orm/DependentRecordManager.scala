package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._
import org.positronicnet.util._
import org.positronicnet.facility._

import scala.collection._

/** Base class for mapping of [[org.positronicnet.orm.ManagedRecord]]s
  * to and from persistent storage, in cases where the storage mapping
  * depends on properties of the record.  (Consider, for instance, a
  * TodoList content provider which provides queries for TodoItems
  * associated with a given list, with the list itself as a URL
  * component, but not just a query for "items in general".)
  *
  * It is conventional to use the
  * [[org.positronicnet.orm.DependentRecordManager]] subclass, which
  * provides additional convenience features, but the base class is
  * available for the inevitable times when the convenience features
  * become inconvenient.
  */

abstract class BaseDependentRecordManager[ T <: ManagedRecord : ClassManifest, Key ]
  ( 
    queryForKey: Key => ContentQuery[_,_], 
    facility: AppFacility 
  )
  extends PrimitiveRecordManager[T]( facility )
{
  def scopeForKey( key: Key ) =
    new DependentTopLevelScope( this, key, queryForKey( key ), facility )
}

abstract class DependentRecordManagerForFields[ TRec <: ManagedRecord : ClassManifest,
                                                Key,
                                                TSrc: ClassManifest ]
    ( queryForKey: Key => ContentQuery[_,_], facility: AppFacility )
  extends BaseDependentRecordManager[ TRec, Key ]( queryForKey, facility )
  with FieldMappingFromStaticNames[ TRec ]
{
  def this( uriForKey: Key => android.net.Uri ) =
    this( key => PositronicContentResolver( uriForKey( key )),
          PositronicContentResolver )

  // Disguised arg to constructor for the FieldMappingFromStaticNames trait

  protected lazy val fieldNamesSrcMap = 
    ReflectUtils.publicStaticValues( classOf [String], 
                                     classManifest[ TSrc ].erasure )
}

class DependentTopLevelScope[ T <: ManagedRecord : ClassManifest, Key ](
    private [orm] val mgr: BaseDependentRecordManager[T,Key],
    val key: Key,
    val baseQuery: ContentQuery[_,_],
    val facility: AppFacility)
  extends BaseNotificationManager( facility )
  with Scope[T]

