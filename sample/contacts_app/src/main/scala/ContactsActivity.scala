package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.content.PositronicContentResolver

import org.positronicnet.notifications.Actions._
import org.positronicnet.orm.Actions._

import android.content.{Context, Intent}
import android.util.{AttributeSet, Log}
import android.os.Bundle
import android.view.View
import android.widget.{TextView, ListView, ImageView, LinearLayout}

import android.accounts.{AccountManager, Account}

import scala.collection.mutable.ArrayBuffer

object ContactsActivityUiBinder extends UiBinder {
  bind[ ImageView, Contact ](
    ( (imageView, contact) =>
      contact.photoQuery.onSuccess { 
        photoRecord =>
          if (photoRecord.isEmpty)
            imageView.setImageDrawable(null)
          else
            imageView.setImageBitmap( photoRecord.thumbnailBitmap )
      }),
    ( (imageView, contact) => contact ) // no update
  )
}

class ContactsActivity 
  extends android.app.ListActivity 
  with PositronicActivityHelpers
  with ActivityViewUtils
  with TypedViewHolder
{
  val listAdapter = new IndexedSeqAdapter[ Contact ]( 
    IndexedSeq.empty,
    R.layout.contact_view_row,
    binder = ContactsActivityUiBinder )

  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources
    setContentView( R.layout.contacts )
    setListAdapter( listAdapter )
    
    useOptionsMenuResource( R.menu.contacts_menu )
    onOptionsItemSelected( R.id.dump_contacts ){ dumpToLog }
    onOptionsItemSelected( R.id.new_contact ) { newContact }
  }

  // Dealing with search state, as an element of activity state.

  var searchState = new ContactsFilterState( R.string.any_contact, None )

  override def createInstanceState = {
    newSearchState( searchState )       // default values as above
    findView( TR.contacts_filter ).setState( searchState )
  }

  override def saveInstanceState( b: Bundle ) = 
    b.putSerializable( "contact_search_state", searchState )

  override def restoreInstanceState( b: Bundle ) = {
    val newStateSlug = b.getSerializable( "contact_search_state" )
    searchState = newStateSlug.asInstanceOf[ ContactsFilterState ]
    newSearchState( searchState )
  }

  // Hook for changes to search state from UI

  def newSearchState( state: ContactsFilterState ) = {
    this.searchState = state
    listAdapter.resetFilter( state.filterFunc ) 
  }

  // The list of contacts itself...

  onResume {
    (Contacts ? Query).onSuccess{ contacts => 
      if (contacts.size > 0) {
        val sortedContacts = contacts.sortBy{ _.displayNamePrimary.toLowerCase }
        this.listAdapter.resetSeq( sortedContacts )
        setListAdapter( this.listAdapter )
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
    startViewingContact( contact )
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
    startEditingRawContacts( Seq (
      if (acct != null)
        new RawContact( accountName = acct.name, accountType = acct.`type` )
      else
        new RawContact))

  def startViewingContact( contact: Contact ) = {
    (RawContacts.forContact( contact ) ? Query).onSuccess { rawContacts =>
      val intent = new Intent( this, classOf[ ViewContactActivity ])
      intent.putExtra( "contact", contact.asInstanceOf[ java.io.Serializable ] )
      intent.putExtra( "raw_contacts", 
                       rawContacts.asInstanceOf[ java.io.Serializable ])
      startActivity( intent )
    }
  }

  def startEditingRawContacts( rawContacts: Seq[ RawContact ] ) = {
    val intent = new Intent( this, classOf[ EditContactActivity ])
    intent.putExtra( "raw_contacts", 
                     rawContacts.asInstanceOf[ java.io.Serializable ])
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

case class ContactsFilterState ( filterChoice: Int,
                                 searchString: Option[ String ] )
{
  def filterFunc: Option[ Contact => Boolean ] = 
    searchString match {
      case None =>
        filterChoice match {
          case R.string.any_contact => None
          case R.string.with_phones => Some( _.hasPhoneNumber )
        }
      case Some( str ) =>
        val strLcase = str.toLowerCase
        filterChoice match {
          case R.string.any_contact =>
            Some( contact => contact.lcDisplayName.contains( strLcase ))
          case R.string.with_phones =>
            Some( contact => contact.hasPhoneNumber &&
                             contact.lcDisplayName.contains( strLcase ))
        }
    }
}

class ContactsFilterView( ctx: Context, attrs: AttributeSet ) 
  extends LinearLayout( ctx, attrs ) 
  with TypedViewHolder 
  with WidgetUtils
{
  private var filterChoice: Int = R.string.any_contact
  private val resources = ctx.getResources

  override def onFinishInflate = {

    super.onFinishInflate
    
    findView( TR.filterChoiceButton ).onClick {
      withChoiceFromDialog[Int]( R.string.choose_filter,
                                 IndexedSeq( R.string.any_contact, 
                                             R.string.with_phones ),
                                 resources.getString( _ ))
      {
        choice =>
          this.filterChoice = choice
          findView( TR.filterChoiceButton ).setText( choice )
          android.util.Log.d( "XXX", "Set filterChoice to " + this.filterChoice )
          searchStateChanged
      }
    }

    findView( TR.searchButton ).onClick {
      if ( findView( TR.contactsSearchBar ).getVisibility == View.GONE ) {
        findView( TR.contactsSearchBar ).setVisibility( View.VISIBLE )
        findView( TR.searchString ).setText( "" )
        searchStateChanged
      }
    }

    findView( TR.endSearchButton ).onClick {
      findView( TR.contactsSearchBar ).setVisibility( View.GONE )
      searchStateChanged
    }

    findView( TR.searchString ).onTextChanged { dummyText =>
      searchStateChanged
    }
  }

  def setState( state: ContactsFilterState ) = {

    this.filterChoice = state.filterChoice

    android.util.Log.d( "XXX", "Set filterChoice to " + state.filterChoice )

    findView( TR.filterChoiceButton ).setText( 
      ctx.getResources.getString( state.filterChoice ))

    state.searchString match {
      case None =>
        findView( TR.contactsSearchBar ).setVisibility( View.GONE )
      case Some( str ) =>
        findView( TR.contactsSearchBar ).setVisibility( View.VISIBLE )
        findView( TR.searchString ).setText( str )
    }
  }

  def getState = 
    ContactsFilterState(
      filterChoice = this.filterChoice,
      searchString =
        findView( TR.contactsSearchBar ).getVisibility match {
          case View.VISIBLE => 
            Some( findView( TR.searchString ).getText.toString )
          case View.GONE =>
            None
        })

  def searchStateChanged =
    getContext.asInstanceOf[ ContactsActivity ].newSearchState( getState )
}

