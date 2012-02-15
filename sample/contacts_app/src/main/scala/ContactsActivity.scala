package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.content.PositronicContentResolver

import org.positronicnet.notifications.Actions._
import org.positronicnet.orm.Actions._

import android.content.{Context, Intent}
import android.util.{AttributeSet, Log}
import android.view.View
import android.widget.{TextView, ListView}

import android.accounts.{AccountManager, Account}

import scala.collection.mutable.ArrayBuffer

class ContactsActivity 
  extends android.app.ListActivity 
  with PositronicActivityHelpers
  with ActivityViewUtils
{
  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources
    setContentView( R.layout.contacts )
    
    useOptionsMenuResource( R.menu.contacts_menu )
    onOptionsItemSelected( R.id.dump_contacts ){ dumpToLog }
    onOptionsItemSelected( R.id.new_contact ) { newContact }
  }

  onResume {
    Contacts ! Fetch { contacts => 
      if (contacts.size > 0) {
        val sortedContacts = contacts.sortBy{ _.displayNamePrimary.toLowerCase }
        setListAdapter( new IndexedSeqAdapter( sortedContacts, 
                                               R.layout.contact_view_row ))
      }
      else {
        // android package IDs don't show up in TypedResources, so...
        val txtv = findViewById( android.R.id.empty ).asInstanceOf[ TextView ]
        txtv.setText( R.string.no_contacts )
      }
    }
  }

  override def onListItemClick( l: ListView, v: View, posn: Int, id: Long ) = {
    val contact = getListAdapter.getItem( posn ).asInstanceOf[Contact]
    RawContacts.forContact( contact ) ! Fetch { rawContacts => 
      if (rawContacts.size == 1)
        startEditingRawContact( rawContacts(0) )
      else {
        val title = R.string.edit_contact_in_account
        withChoiceFromDialog[ RawContact ]( title, rawContacts, _.accountName ){
          startEditingRawContact( _ )
        }
      }
    }
  }

  def newContact = {
    val accounts = AccountManager.get( this ).getAccounts
    if (accounts.size == 0)
      newContactForAccount( null )
    else if (accounts.size == 1)
      newContactForAccount( accounts(0) )
    else {
      val title = R.string.choose_account_for_contact
      withChoiceFromDialog[ Account ]( title , accounts, _.name ){
        newContactForAccount( _ )
      }
    }
  }

  def newContactForAccount( acct: Account ) =
    startEditingRawContact( 
      if (acct != null)
        new RawContact( accountName = acct.name, accountType = acct.`type` )
      else
        new RawContact)

  def startEditingRawContact( rawContact: RawContact ) = {
    val intent = new Intent( this, classOf[ EditRawContactActivity ])
    intent.putExtra( "raw_contact", rawContact )
    startActivity( intent )
  }

  def dumpToLog = {
    Contacts.onThread {
      for ( contact <- Contacts.fetchOnThisThread ) {
        Log.d( "ContactsDump", "Contact: " + contact )
        for ( datum <- contact.data.fetchOnThisThread )
          Log.d("ContactsDump", datum.toString)
      }
      for ( group <- Groups.fetchOnThisThread ) {
        Log.d( "XXX", "Group: " + group.id )
        Log.d( "XXX", group.toString )
      }
    }
  }
}
