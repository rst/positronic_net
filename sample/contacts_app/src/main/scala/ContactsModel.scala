package org.positronicnet.sample.contacts

import org.positronicnet.orm.{RecordId, BatchScopeAction}
import org.positronicnet.orm.Actions._
import org.positronicnet.notifications.Actions._
import org.positronicnet.content.PositronicContentResolver

import scala.collection.mutable.{ArrayBuffer, HashMap}

import android.provider.ContactsContract
import android.util.Log

// Classes that implement the "business logic" of dealing with
// contacts, or at least the things we do with them.

// Class that represents an "edit state" for editing the ContactData
// items associated with a Contact or RawContact

class ContactEditState( val rawContact: RawContact,
                        val initialItems: Seq[ ContactData ] ) 
{
  private var deletedState: ArrayBuffer[ ContactData ] = ArrayBuffer.empty
  private var currentState: HashMap[ RecordId[_], ContactData ] = HashMap.empty
  private val accountInfo = AccountInfo.forRawContact( rawContact )

  def deletedItems: IndexedSeq[ContactData] = deletedState
  def currentItems = currentState.valuesIterator

  def updateItem( rec: ContactData ): Unit = {
    currentState( rec.id ) = 
      if (rec.isUnsaved) 
        rec.setProperty( "rawContactId", rawContact.id )
      else
        rec
  }

  def deleteItem( rec: ContactData ): Unit = {
    currentState.remove( rec.id )
    if (!rec.isNewRecord)
      deletedState += rec
  }

  // "Save" support.  Works through content resolver batch operations,
  // per recommended best practice.

  def save = saveAndThen( null )

  def saveAndThen( callback: => Unit ) = {

    val batch = new BatchScopeAction( ContactsContract.AUTHORITY )

    if ( rawContact.isNewRecord ) {

      batch.add( Save( rawContact ))

      for ( group <- accountInfo.initialGroups )
        updateItem((new GroupMembership).setProperty("groupRowId", group.id))
    }

    for ( item <- deletedState )
      batch.add( Delete( item ))

    for ( item <- currentState.valuesIterator )
      batch.add( Save( item ))

    PositronicContentResolver ! batch.onSuccess{ callback }
  }

  def logIt = {

    if ( rawContact.isNewRecord )
      Log.d( "RawContact state", "New contact: " + rawContact )

    for ( item <- deletedState )
      Log.d( "RawContact state", "Delete " + item )

    for ( item <- currentState.valuesIterator )
      if ( item.isNewRecord )
        Log.d( "RawContact state", "Insert " + item )
      else
        Log.d( "RawContact state", "Update " + item )
  }
}

// Class that represents the value of a "label-or-custom" field.
// These are backed by two underlying fields, one an integer "type"
// (which we generally style "recType" since "type" is a reserved
// word in Scala), and one the custom label, if any.
//
// This also requires a list of possible "types", to support
// changes --- which is a bit of an awkward subject, since the set
// of allowed values depends on the accountType, and the details of
// that are baked into the source code of the standard Contacts app.

case class TypeFieldInfo(
  val recTypes: IndexedSeq[Int],
  val customType: Int,
  val toResource: (Int => Int)
)
{
  val customTypeIdx = recTypes.indexOf( customType )
}

case class TypeField(
  val recType: Int,
  val label:   String,
  val info:    TypeFieldInfo
)
{
  // Sanity-checking:  if we get a recType we didn't expect, shove it in
  // our list, at least for this particular item.  We just assume that
  // our TypeFieldInfo will be able to map it to some resource.  (Which
  // may be the case if the TypeFieldInfo is a sublist of a longer list
  // of stuff supported by the platform.)

  lazy val recTypes = 
    if (info.recTypes.contains( recType ))
      info.recTypes
    else
      info.recTypes :+ recType

  def recType_:=( newType: Int ) = 
    this.copy( recType = newType, 
               label = (if (newType == info.customType) label else null ))

  def label_:=( s: String ) = 
    this.copy( recType = info.customType, label = s )

  def isCustom = {recType == info.customType}

  // Utility routines for translating these values to displayable
  // strings, using "Res.ources" from the Widgets.

  def displayString = displayStringOfRecType( recType )

  def displayStrings = recTypes.map{ displayStringOfRecType(_) }

  def selectedStringIdx = recTypes.indexOf( recType )

  def displayStringOfRecType( recType: Int ) =
    if (recType == info.customType && label != null)
      label
    else {
      val str = Res.ources.getString( info.toResource( recType ))
      if (str != null)
        str
      else
        "Unknown type " + recType
    }
}

// Information on account types...
// The minimal stuff here.  More to come...

object AccountInfo {

  def forRawContact( rawc: RawContact ) =
    rawc.accountType match {
      case "com.google" => 
        new GoogleAccountInfo( rawc.accountType, rawc.accountName )
      case _ => 
        new OtherAccountInfo( rawc.accountType, rawc.accountName )
    }
}

abstract class AccountInfo {
  def initialGroups: Seq[ Group ]
}

class OtherAccountInfo( acctType: String, acctName: String ) 
  extends AccountInfo
{
  def initialGroups = Seq.empty
}

class GoogleAccountInfo( acctType: String, acctName: String ) 
  extends AccountInfo
{
  // The following is aping some of the logic in the official contacts
  // app --- we attempt to add new contacts in a Google account to its
  // "My Contacts" group, if such a thing is to be found.  
  // 
  // The business of looking for it by name is a kludge, but that's
  // what's in model/GoogleSource.java in the Gingerbread version of
  // the official Contacts app; ICS instead looks at an AUTO_ADD
  // column which isn't referred to in the API docs.
  //
  // We do not yet attempt to *create* the group if it isn't found.
  // The Gingerbread app will do that; no similar functionality is
  // obviously present in the ICS version.  Even the version we have
  // here is potentially subject to a race condition; we start loading
  // the group in the background when we start editing, and hope it
  // will be loaded on save.  This is 

  val myContactsName = "System Group: My Contacts"

  var myContactGroupSeq: Seq[Group] = Seq.empty

  def initialGroups = myContactGroupSeq

  // Try to find our default group, in the background

  val groupQuery = Groups.whereEq( "accountName" -> acctName,
                                   "accountType" -> acctType,
                                   "title" -> myContactsName )

  groupQuery ! Fetch { groups => 
    myContactGroupSeq = groups }
}
