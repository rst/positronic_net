package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.notifications.Actions._
import org.positronicnet.content.PositronicContentResolver

import android.util.Log

class EditRawContactActivity
  extends PositronicActivity( layoutResourceId = R.layout.edit_contact )
  with TypedViewHolder
{
  var state: ContactEditState = null    // set up onCreate, valid thereafter

  onCreate {

    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources

    useOptionsMenuResource( R.menu.edit_contact_menu )
    onOptionsItemSelected( R.id.save_raw_contact ) { doSave }

    val rawContact = 
      getIntent.getSerializableExtra( "raw_contact" ).asInstanceOf[ RawContact ]

    rawContact.data ! Fetch { data => 
      this.state = new ContactEditState( rawContact, data )
      val editors = findView( TR.editors )
      for (i <- Range(0, editors.getChildCount)) {
        val editor = editors.getChildAt(i).asInstanceOf[ CategoryDisplay[_] ]
        editor.bind( state )
      }
    }
  }

  def doSave = {
    val editors = findView( TR.editors )
    for (i <- Range(0, editors.getChildCount)) {
      editors.getChildAt(i) match {
        case cd: CategoryDisplay[_] => cd.updateState
        case _ => 
      }
    }
    state.logIt
    state.saveAndThen{ this.runOnUiThread{ finish }}
  }
}
