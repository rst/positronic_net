package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.notifications.Actions._
import org.positronicnet.notifications.Future
import org.positronicnet.content.PositronicContentResolver

import android.util.Log
import android.os.Bundle
import android.content.Context
import android.view.{View, LayoutInflater}

// Superclass of activities which manipulate data pertaining to an
// individual aggregated contact.

abstract class AggregatedContactActivity( layoutResourceId: Int )
  extends PositronicActivity( layoutResourceId = layoutResourceId )
  with TypedViewHolder
  with ActivityResultDispatch           // for phone/sms... & photo-edit widgets
{
  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources
  }

  // Methods that must be defined by a subclass to set up for a new
  // AggregateContactEditState, or to synch it with updated state
  // reflected in displayed widgets.

  def bindContactState: Unit
  def syncContactState: Unit

  // Management of our edit state across the Activity lifecycle,
  // including suspend/recreate cycles (due to orientation changes,
  // or whatever else).

  private var currentState: AggregateContactEditState = null
  def contactState = currentState       // reader only

  private def setState( state: AggregateContactEditState ) = {
    this.currentState = state
    this.bindContactState
  }

  // Invoked by our helpers if we are *not* restoring state from a prior
  // invocation, and need to do setup.  (From postCreate, but only if
  // onRestoreInstanceState was *not* invoked.)

  override def createInstanceState = {

    val contactsSlug = getIntent.getSerializableExtra( "raw_contacts" )
    val rawContacts = contactsSlug.asInstanceOf[ Seq[ RawContact ]]
    val noData: IndexedSeq[ContactData] = IndexedSeq.empty
    val dataQueries =
      rawContacts.map { rawContact =>
        if (rawContact.isNewRecord) 
          Future( noData )               // Immediate empty query result
        else 
          (rawContact.data ? Query)      // Background query for data
      }

    // ... then when all are finished, pair them up with their contacts, and
    // post further setup onto this thread.
    //
    // Also, if the view has a scrollable region (all of our child classes
    // do), we always want it forced to the top here, so do that.

    Future.sequence( dataQueries ).onSuccess { data => {
      this.setState( new AggregateContactEditState( rawContacts.zip( data )))
      val scroller = findView( TR.scroller )
      if (scroller != null)
        scroller.fullScroll( View.FOCUS_UP )
    }}
  }

  // The usual save/restore pair, to handle suspensions.

  override def saveInstanceState( b: Bundle ) = {
    syncContactState
    b.putSerializable( "contact_edit_state", contactState )
  }

  override def restoreInstanceState( b: Bundle ) =
    setState(
      b.getSerializable( "contact_edit_state" ).asInstanceOf[ 
        AggregateContactEditState ])

}
