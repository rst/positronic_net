package org.positronicnet.sample.contacts

import org.positronicnet.orm.{RecordId, BatchScopeAction}
import org.positronicnet.orm.Actions._

import scala.collection.mutable.ArrayBuffer

import android.provider.ContactsContract
import android.util.Log

// Class that represents an "edit state" for editing the ContactData
// items associated with a Contact or RawContact

class ContactEditState( val rawContact: RawContact,
                        initialItems: Seq[ ContactData ] ) 
  extends Serializable
{
  private var deletedState = new ArrayBuffer[ ContactData ]
  private var currentState = new ArrayBuffer[ ContactData ]
  @transient private lazy val accountInfo = 
    AccountInfo.forRawContact( rawContact )

  currentState ++= initialItems.filter{
    case unknown: UnknownData => false
    case _ => true
  }

  def deletedItems: IndexedSeq[ContactData] = deletedState
  def currentItems: IndexedSeq[ContactData] = currentState

  def updateItem( rec: ContactData ): Unit = {

    val tweakedRec =
      if (rec.isUnsaved) 
        rec.setProperty( "rawContactId", rawContact.id )
      else
        rec
    
    val idx = currentState.indexWhere( _.id == rec.id )
    
    if (idx < 0) 
      currentState += tweakedRec
    else 
      currentState( idx ) = tweakedRec
  }

  def deleteItem( rec: ContactData ): Unit = {
    val idx = currentState.indexWhere( _.id == rec.id )

    if (idx >= 0) 
      currentState.remove( idx )

    if (!rec.isNewRecord) 
      deletedState += rec
  }

  // "Save" support.  Yields a content resolver batch operation that
  // does the job in a single round, per recommended best practice.

  def saveBatch = {

    val batch = new BatchScopeAction( ContactsContract.AUTHORITY )

    if ( rawContact.isNewRecord ) {

      batch.add( Save( rawContact ))

      for ( group <- accountInfo.initialGroups )
        updateItem((new GroupMembership).setProperty("groupRowId", group.id))
    }

    for ( item <- deletedState )
      batch.add( Delete( item ))

    for ( item <- currentState )
      if (!item.isEmpty)
        batch.add( Save( item ))
      else if (!item.isNewRecord)
        batch.add( Delete( item ))

    batch
  }

  // Determine what categories for a given type are available...
  // All of them, for the moment.

  def availableCategories( item: ContactData ) =
    accountInfo.dataKinds.get( item.typeTag ) match {
      case Some( kindInfo ) => kindInfo.categories
      case None => IndexedSeq.empty
    }

  // Data kind info for an item, if any

  def dataKindInfo( item: ContactData ) =
    accountInfo.dataKinds.get( item.typeTag )

  // Prepare a new item for insertion, if we have room for one.
  // May return None, in which case, insertion is disallowed
  // (most likely, because we're up against some limit).
  // But not yet.

  def prepareForInsert( item: ContactData ) =

    item match {

      case itemWithCategory: ContactDataWithCategoryLabel =>
        accountInfo.dataKinds.get( item.typeTag ) map { info =>
          item.setProperty[ Int ]("categoryTag", info.categories(0).tag )
        }

      case _ => Some( item )
    }

  // Dump current state to the log...

  def logIt = {

    if ( rawContact.isNewRecord )
      Log.d( "RawContact state", "New contact: " + rawContact )

    for ( item <- deletedState )
      Log.d( "RawContact state", "Delete " + item )

    for ( item <- currentState )
      if ( item.isNewRecord )
        Log.d( "RawContact state", "Insert " + item )
      else
        Log.d( "RawContact state", "Update " + item )
  }
}

