package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.content.PositronicContentResolver

import org.positronicnet.notifications.Actions._
import org.positronicnet.orm.Actions._

import android.util.Log
import android.view.View
import android.view.KeyEvent
import android.widget.TextView

import scala.collection.mutable.ArrayBuffer

object ContactsUiBinder extends UiBinder

class ContactsActivity 
  extends android.app.ExpandableListActivity with PositronicActivityHelpers
{
  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources
    setContentView( R.layout.contacts )
    
    useOptionsMenuResource( R.menu.contacts_menu )
    onOptionsItemSelected( R.id.dump_contacts ){ dumpToLog }
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
                                      R.layout.rawcontact_view_row ))
  }}}}

  def dumpToLog = {
    Contacts.onThread {
      for ( contact <- Contacts.fetchOnThisThread ) {
        Log.d( "XXX", "Contact: " + contact )
        for ( datum <- contact.data.fetchOnThisThread ) {
          datum match {
            case phone: Phone => 
              Log.d("XXX","  Phone: "+ phone.displayType +" "+ phone.number)
            case email: Email =>
              Log.d("XXX","  Email: "+ email.displayType +" "+ email.address)
            case name: StructuredName =>
              Log.d("XXX", name.toString )
            case membership: GroupMembership =>
              Log.d("XXX","  Group: " + membership.groupRowId.fetchOnThisThread)
            case stuff: UnknownData =>
              Log.d("XXX","  " + stuff.mimetype + " " + stuff.data1)
          }
        }
      }
      for ( group <- Groups.fetchOnThisThread ) {
        Log.d( "XXX", "Group: " + group.id )
        Log.d( "XXX", group.toString )
      }
    }
  }
}
