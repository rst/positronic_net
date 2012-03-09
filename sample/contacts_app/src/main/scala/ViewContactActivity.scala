package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.notifications.Actions._
import org.positronicnet.notifications.Future
import org.positronicnet.content.PositronicContentResolver

import android.util.Log
import android.os.Bundle
import android.content.{Context, Intent}
import android.view.{View, LayoutInflater}

class ViewContactActivity
  extends AggregatedContactActivity( layoutResourceId = R.layout.view_contact )
{
  onCreate {
    findView( TR.edit_button ).onClick {
      val rawContacts = contactState.rawContacts
      val intent = new Intent( this, classOf[ EditContactActivity ])
      intent.putExtra( "raw_contacts", 
                       rawContacts.asInstanceOf[ java.io.Serializable ])
      startActivity( intent )

      // I *never* want to view again after a save here, so...

      finish
    }
    findView( TR.join_split_button ).onClick {
      toastLong( "not yet ...")
    }
  }

  def bindContactState = {
    val contactSlug = getIntent.getSerializableExtra( "contact" )
    val contact = contactSlug.asInstanceOf[ Contact ]
    ContactsActivityUiBinder.show( contact, findView( TR.contact_general ) )
    findView( TR.contact_data_items ).bind( contactState.aggregatedData )
  }

  def syncContactState = ()             // no change...
}




