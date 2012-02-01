package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.notifications.Actions._
import org.positronicnet.content.PositronicContentResolver

import android.util.Log
import android.accounts.{AccountManager, Account}
import android.app.AlertDialog
import android.content.DialogInterface  // android.content?!

class EditRawContactActivity
  extends PositronicActivity with TypedViewHolder
{
  var state: ContactEditState = null    // set up onCreate, valid thereafter

  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources

    useOptionsMenuResource( R.menu.edit_contact_menu )
    onOptionsItemSelected( R.id.save_raw_contact ) { doSave }
  }

  def bindState( state: ContactEditState ) = {
    this.state = state
    val editors = findView( TR.editors )
    for (i <- Range(0, editors.getChildCount)) {
      val editor = editors.getChildAt(i).asInstanceOf[ CategoryDisplay[_] ]
      editor.bind( state )
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
    state.saveAndThen{ this.runOnUiThread{ state.logIt; finish }}
  }
}

class EditExistingContactActivity
  extends EditRawContactActivity
{
  onCreate {
    setContentView( R.layout.edit_contact )

    val rawContact = 
      getIntent.getSerializableExtra( "raw_contact" ).asInstanceOf[ RawContact ]

    rawContact.data ! Fetch { data => 
      bindState( new ContactEditState( rawContact, data ))
    }
  }
}

class EditNewContactActivity
  extends EditRawContactActivity
{
  onResume {
    if (state == null) {
      val accounts = AccountManager.get( this ).getAccounts
      if (accounts.size == 0)
        setUpForAccount( null )
      else if (accounts.size == 1)
        setUpForAccount( accounts(0) )
      else {
        val dbuilder = new AlertDialog.Builder( this )
        dbuilder.setTitle( R.string.choose_account_for_contact )
        dbuilder.setItems( 
          accounts.map{ _.name.asInstanceOf[java.lang.CharSequence] },
          new DialogInterface.OnClickListener {
            def onClick( dialog: DialogInterface, idx: Int ) = {
              setUpForAccount( accounts( idx ))
            }
          }
        )
        dbuilder.create.show
      }
    }
  }

  def setUpForAccount( acct: Account ) = {

    val contact = 
      if (acct != null)
        new RawContact( accountName = acct.name, accountType = acct.`type` )
      else
        new RawContact

    setContentView( R.layout.edit_contact )
    bindState( new ContactEditState( contact, Seq.empty ))
  }
}
