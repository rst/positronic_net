package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.content.PositronicContentResolver

import org.positronicnet.notifications.Actions._
import org.positronicnet.orm.Actions._

import android.content.{Context, Intent}
import android.util.{AttributeSet, Log}
import android.view.View
import android.widget.{TextView, ExpandableListView}

import android.accounts.{AccountManager, Account}
import android.app.AlertDialog
import android.content.DialogInterface  // android.content?!

import scala.collection.mutable.ArrayBuffer

object ActivityUiBinder extends UiBinder {
  bind[ RawContactView, RawContact ]( 
    (_.setRawContact(_)), 
    ((x,y) => throw new RuntimeException( "rawcontact set not defined!" )))
}

class RawContactView( ctx: Context, attrs: AttributeSet )
  extends PositronicTextView( ctx, attrs )
{
  var rawc: RawContact = null

  def setRawContact( r: RawContact ) = {
    rawc = r
    setText( if (r.accountName != null) r.accountName else "Phone-only" )
  }

  onClick {
    val intent = new Intent( getContext, classOf[ EditRawContactActivity ])
    intent.putExtra( "raw_contact", rawc )
    getContext.startActivity( intent )
  }
}

class ContactsActivity 
  extends android.app.ExpandableListActivity 
  with PositronicActivityHelpers
{
  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources
    setContentView( R.layout.contacts )
    
    useOptionsMenuResource( R.menu.contacts_menu )
    onOptionsItemSelected( R.id.dump_contacts ){ dumpToLog }
    onOptionsItemSelected( R.id.new_contact ) { newContact }

    getExpandableListView.setOnChildClickListener(this) // not automatic?!
  }

  onResume {

    // There's no obvious way to retrieve a join of Contacts to
    // RawContacts (though for a real cheat, we could try doing it
    // through ContactData, which has implicit joins to both).  So,
    // we instead do separate fetches and do the join here.
    //
    // Could load an initial data set when just contacts are received,
    // in order to show the users something quickly, if the full
    // RawContact fetch causes a perceptible pause.

    Contacts ! Fetch { contacts =>
      RawContacts ! Fetch { rawContacts => {

        val dataByContactId = 
          contacts.collect{ case contact:Contact => 
            (contact.id, (contact, new ArrayBuffer[RawContact] )) }.toMap
        
        for (rawContact <- rawContacts)
          if (!rawContact.deleted)
            dataByContactId( rawContact.contactId )._2 += rawContact

        val pairs = dataByContactId.values.toIndexedSeq 
        val data = pairs.sortBy{ _._1.displayNamePrimary.toLowerCase }

        setListAdapter(
          new IndexedSeqGroupAdapter( data,
                                      R.layout.contact_view_row,
                                      R.layout.rawcontact_view_row,
                                      ActivityUiBinder ))
  }}}}

  def newContact = {
    val accounts = AccountManager.get( this ).getAccounts
    if (accounts.size == 0)
      newContactForAccount( null )
    else if (accounts.size == 1)
      newContactForAccount( accounts(0) )
    else {
      val dbuilder = new AlertDialog.Builder( this )
      dbuilder.setTitle( R.string.choose_account_for_contact )
      dbuilder.setItems( 
        accounts.map{ _.name.asInstanceOf[java.lang.CharSequence] },
        new DialogInterface.OnClickListener {
          def onClick( dialog: DialogInterface, idx: Int ) = {
            newContactForAccount( accounts( idx ))
          }
        }
      )
      dbuilder.create.show
    }
  }

  def newContactForAccount( acct: Account ) = {

    val rawContact = 
      if (acct != null)
        new RawContact( accountName = acct.name, accountType = acct.`type` )
      else
        new RawContact

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
