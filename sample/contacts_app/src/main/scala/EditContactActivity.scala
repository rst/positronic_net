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
  extends AggregatedContactActivity( layoutResourceId = R.layout.edit_contact )
{
  onCreate {
    useOptionsMenuResource( R.menu.edit_contact_menu )
    onOptionsItemSelected( R.id.save_raw_contact ) { doSave }
  }

  // Loading a state into our editor widgets
  // (invoked by AggregatedContactActivity base code, on start or restart)

  def bindContactState = {

    val editorContainer = findView( TR.raw_contact_editors )
    editorContainer.removeAllViews

    val inflater = getSystemService( Context.LAYOUT_INFLATER_SERVICE )
      .asInstanceOf[ LayoutInflater ]

    for (rawState <- contactState.rawContactEditStates) {
      val rawEditor = inflater.inflate( R.layout.edit_raw_contact, 
                                        editorContainer, false )
      rawEditor.asInstanceOf[ RawContactEditor ].bindState( rawState )
      editorContainer.addView( rawEditor )
    }
  }

  // Updating the state from what's displayed in the editor widgets.
  // (Invoked by AggregatedContactActivity code on save, and by doSave below.)

  def syncContactState = {
    val editorContainer = findView( TR.raw_contact_editors )
    for (ed <- editorContainer.childrenOfType[ RawContactEditor ])
      ed.updateState
  }

  // Doing a save

  def doSave = {
    syncContactState
    val batch = contactState.saveBatch
    PositronicContentResolver ! batch.onSuccess{ finish }.onFailure{ 
      toastShort("Error saving; see log") }
  }
}




