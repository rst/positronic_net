package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.notifications.Actions._
import org.positronicnet.notifications.Future
import org.positronicnet.content.PositronicContentResolver

import android.util.Log
import android.os.Bundle
import android.content.Context
import android.view.{View, LayoutInflater}

class EditContactActivity
  extends PositronicActivity( layoutResourceId = R.layout.edit_contact )
  with TypedViewHolder
  with ActivityResultDispatch           // for photo edit widgetry
{
  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources

    useOptionsMenuResource( R.menu.edit_contact_menu )
    onOptionsItemSelected( R.id.save_raw_contact ) { doSave }
  }

  // Management of our edit state across the Activity lifecycle,
  // including suspend/recreate cycles (due to orientation changes,
  // or whatever else).

  var state: AggregateContactEditState = null

  override def createInstanceState = {

    // Have no saved instance state.  Create it.  Retrieve our raw
    // contacts, and fire off background queries for their data...

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

    Future.sequence( dataQueries ).onSuccess { data => {
      this.bindState( new AggregateContactEditState( rawContacts.zip( data )))
      findView( TR.scroller ).fullScroll( View.FOCUS_UP )
    }}
  }

  override def saveInstanceState( b: Bundle ) = {
    syncState
    b.putSerializable( "contact_edit_state", this.state )
  }

  override def restoreInstanceState( b: Bundle ) = {
    val state = b.getSerializable( "contact_edit_state" )
    this.bindState( state.asInstanceOf[ AggregateContactEditState ] )
  }

  // Loading a state into our editor widgets

  def bindState( state: AggregateContactEditState ) = {

    this.state = state

    val editorContainer = findView( TR.raw_contact_editors )
    editorContainer.removeAllViews

    val inflater = getSystemService( Context.LAYOUT_INFLATER_SERVICE )
      .asInstanceOf[ LayoutInflater ]

    for (rawState <- state.rawContactEditStates) {
      val rawEditor = inflater.inflate( R.layout.edit_raw_contact, 
                                        editorContainer, false )
      rawEditor.asInstanceOf[ RawContactEditor ].bindState( rawState )
      editorContainer.addView( rawEditor )
    }
  }

  // Updating the state from what's displayed in the editor widgets.

  def syncState = {
    val editorContainer = findView( TR.raw_contact_editors )
    for (ed <- editorContainer.childrenOfType[ RawContactEditor ])
      ed.updateState
  }

  // Doing a save

  def doSave = {
    syncState
    PositronicContentResolver ! state.saveBatch.onSuccess{ finish }.onFailure{ 
      toastShort("Error saving; see log") }
  }
}




