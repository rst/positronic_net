package org.positronicnet.ui

import _root_.android.content.Context
import _root_.android.view.LayoutInflater
import _root_.android.view.View
import _root_.android.view.ViewGroup
import _root_.android.util.Log

import org.positronicnet.notifications.Notifier
import org.positronicnet.notifications.DataStream

import org.positronicnet.content.PositronicCursor // for CursorSourceAdapter
import _root_.android.database.Cursor

import scala.collection.mutable.ArrayBuffer

/** Adapter for cursors produced by PositronicDb queries.
  * Automatically handles a fair amount of the usual typecasting
  * gubbish...
  */
abstract class CursorSourceAdapter[T <: AnyRef]( 
  activity: PositronicActivityHelpers,
  converter: PositronicCursor => T,
  source: Notifier[PositronicCursor] = null,
  itemViewResourceId: Int = 0
)
 extends _root_.android.widget.CursorAdapter( activity, null )
{
  protected var inflater: LayoutInflater = null

  if (source != null) {
    activity.manageListener( this, source ) {
      this.changeCursor( _ )
    }
  }

  def newView( context: Context, 
               cursor: android.database.Cursor,
               parent: ViewGroup ): View =
  {
    if (itemViewResourceId == 0)
      throw new RuntimeException( "QueryAdapter with itemViewResourceId unset"+
                                  " and newView not overridden" )
    if (inflater == null) {
      inflater = 
        parent.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
          .asInstanceOf[LayoutInflater]
    }

    return inflater.inflate( itemViewResourceId, parent, false )
  }
                 
  override def bindView( view: View, context: Context, cursor: Cursor ) = {
    val item = converter( cursor.asInstanceOf[ PositronicCursor ] )
    bindItem( view, item )
  }

  def bindItem( view: View, item: T )

  override def getItem( posn: Int ): T = {
    val baseValue = super.getItem( posn )
    if (baseValue == null)
      return null.asInstanceOf[T]
    else
      return converter( baseValue.asInstanceOf[ PositronicCursor ])
  }
}

/**
  * Adapter for Scala `IndexedSeq`s.
  *
  * Supports `newView` and `bindView` methods, analogous to those
  * provided by the base framework's `CursorAdapter` (though
  * `newView` takes only the parent `ViewGroup` as an argument).
  *
  * Note that the `T <: Object` restriction is needed so that
  * our `getItem( _: Int ):T` is compatible with the declared
  * `getItem( _: Int ): java.lang.Object` in the Adapter interface.
  * So, if you really want an adapter for an `IndexedSeq[Long]`,
  * you're on your own.
  *
  * The `itemViewResourceId` and `itemTextResourceId` constructor
  * arguments are optional, but are used by the default implementations
  * of `newView` and `bindView`, q.v., to handle simple cases with a minimum
  * of extra code.
  */

class IndexedSeqAdapter[T <: Object](protected var seq:IndexedSeq[T] = new ArrayBuffer[T],
                                     itemViewResourceId: Int = 0, 
                                     binder: UiBinder = UiBinder
                                    ) 
  extends _root_.android.widget.BaseAdapter 
{
  protected var inflater: LayoutInflater = null
  protected var filterOpt: Option[ T => Boolean ] = None

  protected var realSeq = seq

  private def notifySomethingChanged = {
    this.realSeq = filterOpt match {
      case Some( func ) => seq.filter( func )
      case None => seq
    }
    notifyDataSetChanged
  }

  /** Method to change filtering */

  def resetFilter( newFilterOpt: Option[ T => Boolean ] ) = {
    this.filterOpt = newFilterOpt
    notifySomethingChanged
  }

  /** Method to reset the sequence if a new copy was (or might have been)
    * loaded off the UI thread.
    */

  def resetSeq( newSeq: IndexedSeq[T] ) = {
    seq = newSeq
    notifySomethingChanged
  }

  /** Get a view to use for the given position.  Ordinarily delegates to the
    * `newView` and `bindView` methods, q.v.
    */

  def getView( position: Int, convertView: View, parent: ViewGroup ):View = {

    val view = 
      if (convertView != null) {
        convertView
      }
      else {
        if (inflater == null) {
          inflater = 
            parent.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
             .asInstanceOf[LayoutInflater]
        }
        newView( parent )
      }

    bindView( view, getItem( position ))
    return view
  }

  /** Create a new view to display items (if our `AdapterView`'s pool has
    * no spares).
    *
    * If it's not overridden, and if an `itemViewResourceId` was supplied
    * to the constructor, the default implementation will use a layout
    * inflater to inflate that resource, and return the result.
    */

  def newView( parent: ViewGroup ): View = {
    assert( itemViewResourceId != 0 )
    inflater.inflate( itemViewResourceId, parent, false )
  }

  /** Make one of the views resulting from `newView` display a particular
    * `item`.  The default implementation uses the
    * [[org.positronicnet.ui.UiBinder]] object passed in as a constructor
    * argument, for which the default is the [[org.positronicnet.ui.UiBinder]]
    * singleton.
    *
    * If the `view` is a `TextView`, and no other arrangements have been
    * made, this will effectively do `view.setText(item.toString)`.  See
    * the documentation on [[org.positronicnet.ui.UiBinder]] for how to
    * easily make it do something smarter (e.g., loading views with the
    * values of properties named by their resource IDs).
    */

  def bindView( view: View, item: T ) = binder.show( item, view )

  /** Get the n'th item from the current sequence */

  def getItem(position: Int):T = realSeq(position)

  /** Get the id of the n'th item from the current sequence */

  def getItemId(position: Int) = position

  /** Get number of items in the current sequence */

  def getCount = realSeq.size
}

/**
  * Adapter for [[org.positronicnet.notifications.Notifier]]s which
  * manage (and report changes to) Scala `IndexedSeq`s.
  *
  * Like [[org.positronicnet.ui.IndexedSeqAdapter]], except that it wires
  * itself up to automatically be notified of changes within the lifetime
  * of the given `activity`.
  */

class IndexedSeqSourceAdapter[T <: Object](activity: PositronicActivityHelpers,
                                           source: Notifier[IndexedSeq[T]],
                                           itemViewResourceId: Int = 0, 
                                           binder: UiBinder = UiBinder) 
  extends IndexedSeqAdapter[T]( itemViewResourceId = itemViewResourceId,
                                binder = binder )
{
  activity.manageListener( this, source ) { resetSeq( _ ) }
}

/**
  * Adapter for [[org.positronicnet.notifications.DataStream]]s of
  * Scala `IndexedSeq`s.  The adapter starts out with the stream's initial
  * (or current) value, and refreshes itself as and when updates are
  * provided.
  *
  * Like [[org.positronicnet.ui.IndexedSeqAdapter]], except that it wires
  * itself up to automatically be notified of changes as reported by
  * the DataStream.
  */

class IndexedSeqDataStreamAdapter[ T <: Object ](
  stream:             DataStream[ IndexedSeq[ T ]],
  itemViewResourceId: Int                            = 0,
  binder:             UiBinder                       = UiBinder
)
extends IndexedSeqAdapter[T]( 
  itemViewResourceId = itemViewResourceId,
  binder             = binder
)
{
  stream.withValues { resetSeq( _ ) }
}

/**
 * Simple adapter for expandable lists.  Takes an underlying data set,
 * which is an `IndexedSeq[(GroupItem, IndexedSeq[ChildItem])]`, and
 * does the obvious, using a passed-in UiBinder to bind the views.
 */

class IndexedSeqGroupAdapter[ GroupItem <: Object, ChildItem <: Object ]
  (protected var data: IndexedSeq[(GroupItem, IndexedSeq[ChildItem])],
   groupViewResourceId: Int = 0, 
   childViewResourceId: Int = 0,
   binder: UiBinder = UiBinder
  )
  extends android.widget.BaseExpandableListAdapter
{
  protected var inflater: LayoutInflater = null

  protected def getInflater( view: android.view.View ) = {
    if (inflater == null) {
      inflater = 
        view.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
          .asInstanceOf[LayoutInflater]
    }
    inflater
  }

  def reset( newData: IndexedSeq[(GroupItem, IndexedSeq[ChildItem])] ) = {
    this.data = newData
    notifyDataSetChanged
  }

  def getChild( grpPos: Int, chldPos: Int ) = data (grpPos)._2 (chldPos) 

  def getChildId( grpPos: Int, chldPos: Int ) = chldPos

  def getChildrenCount( grpPos: Int ) = data (grpPos)._2.size

  def getChildView( grpPos: Int, chldPos: Int, isLastChild: Boolean,
                    convertView: View, parent: ViewGroup ): View = 
  {
    val view = if (convertView != null) convertView 
               else newChildView (grpPos, chldPos, parent)

    bindChildView (view, getChild (grpPos, chldPos), isLastChild )

    view
  }

  def newChildView( grpPos: Int, chldPos: Int, parent: ViewGroup ) = {
    assert( childViewResourceId != 0 )
    getInflater( parent ).inflate( childViewResourceId, parent, false )
  }

  def bindChildView (view: View, child: ChildItem, isLastItem: Boolean) =
    binder.show (child, view)

  def getGroup( grpPos: Int ) = data (grpPos)._1

  def getGroupId( grpPos: Int ) = grpPos

  def getGroupCount = data.size

  def getGroupView( grpPos: Int, isExpanded: Boolean, 
                    convertView: View, parent: ViewGroup): View = 
  {
    val view = if (convertView != null) convertView 
               else newGroupView (grpPos, parent)

    bindGroupView (view, getGroup (grpPos))

    view
  }

  def newGroupView( grpPos: Int, parent: ViewGroup ) = {
    assert( groupViewResourceId != 0 )
    getInflater( parent ).inflate( groupViewResourceId, parent, false )
  }

  def bindGroupView (view: View, group: GroupItem) =
    binder.show (group, view)

  def hasStableIds = false

  def isChildSelectable( grpPos: Int, chldPos: Int ) = false
}
