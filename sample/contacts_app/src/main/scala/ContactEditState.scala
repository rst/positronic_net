package org.positronicnet.sample.contacts

import org.positronicnet.orm.{RecordId, BatchScopeAction}
import org.positronicnet.orm.Actions._

import scala.collection.mutable.{ArrayBuffer, HashMap}

import android.provider.ContactsContract
import android.util.Log

// Class that represents an "edit state" for editing ContactData
// associated with an aggregated Contact.  Most operations are
// managed by the subsidiary RawContactEditStates.  But we need
// some functionality here for managing "superprimary" status
// (i.e., "this is the primary data record of this type across 
// the whole aggregated contact"), since setting it on a single
// record requires clearing it everywhere else, including records
// in other RawContacts.

class AggregateContactEditState( rawData: Seq[(RawContact, Seq[ContactData])] )
  extends Serializable
{
  val rawContactEditStates = 
    for ((rawc, data) <- rawData)
    yield new RawContactEditState( this, rawc, data )

  val newSuperPrimary = new HashMap[ Class[_], RecordId[_] ]

  def markSuperPrimary( datum: ContactData ) = 
    newSuperPrimary( datum.getClass ) = datum.id

  // "Save" support.  Yields a content resolver batch operation that
  // does the job in a single round, per recommended best practice.
  //
  // (Note that if one of the RawContacts has been dinked by a sync,
  // we will at least attempt to dink it further, on the theory that
  // the data that the user is most likely to want is the one that
  // they just entered on their own phone.)

  def saveBatch = {

    // First a sanity check.  If the user decided to mark a record
    // "super-primary" (i.e., the one to use above all others in this
    // aggregated contact), and they also decided to delete the same
    // one, just forget all about it.  (This leaves all primary and
    // "super-primary" flags in their prior state, which makes as
    // much sense as anything else I can think of doing.)

    for ( rawState <- rawContactEditStates;
          deletedItem <- rawState.deletedItems )
      if ( newSuperPrimary.get( deletedItem.getClass ) == deletedItem.id )
        newSuperPrimary.remove( deletedItem.getClass )

    // Now, batch up our updates.

    val batch = new BatchScopeAction( ContactsContract.AUTHORITY )

    for ( rawState <- rawContactEditStates )
      rawState.saveToBatch( batch, newSuperPrimary )

    batch
  }
}

// Class that represents an "edit state" for editing the ContactData
// items associated with or RawContact

class RawContactEditState( val aggregateEditState: AggregateContactEditState,
                           val rawContact: RawContact,
                           initialItems: Seq[ ContactData ] ) 
  extends Serializable
{
  private var deletedState = new ArrayBuffer[ ContactData ]
  private var currentState = new ArrayBuffer[ ContactData ]
  @transient lazy val accountInfo = AccountInfo.forRawContact( rawContact )

  currentState ++= initialItems.filter{
    case unknown: UnknownData => false
    case _ => true
  }

  def accountName = rawContact.accountName

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

  def markSuperPrimary( rec: ContactData ): Unit =
    aggregateEditState.markSuperPrimary( rec )

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

  def prepareForInsert( item: ContactData ) = {

    item match {

      case itemWithCategory: ContactDataWithCategoryLabel =>
        accountInfo.dataKinds.get( item.typeTag ) map { info =>
          item.setProperty[ Int ]("categoryTag", info.categories(0).tag )
        }

      case _ => Some( item )
    }
  }

  // "Save" support...

  def saveToBatch( batch: BatchScopeAction, 
                   newSuperPrimary: HashMap[ Class[_], RecordId[_] ] ) = 
  {
    // First off, if we're adding a contact, write out the RawContact
    // record itself, and memberships in its account's initial groups
    // (if any).

    if ( rawContact.isNewRecord ) {

      batch.add( Save( rawContact ))
      
      accountInfo.initialGroupQuery.block

      accountInfo.initialGroupQuery.onSuccess { groups =>
        for ( group <- groups ) {
          val gm = (new GroupMembership)
            .setProperty("groupRowId", group.id)
            .setProperty("rawContactId", rawContact.id )
          batch.add( Save( gm ))
        }
      }
    }

    // Next, delete any items the user decided to kill.

    for ( item <- deletedItems )
      batch.add( Delete( item ))

    // Now, write out new or updated records.  This is where the oddball
    // "super-primary" rules come into play --- if the user marked records
    // to be considered "super-primary", the last selection of a given
    // type gets the flag *set*, and all others get it *cleared*.
    //
    // Otherwise, we leave it all as-is.

    for ( item <- currentItems )
      if (item.isEmpty) {
        if (!item.isNewRecord)
          batch.add( Delete( item ))
      }
      else {
        val newSuperPrimaryId = 
          newSuperPrimary.getOrElse( item.getClass, null )

        batch.add( Save(
          if ( newSuperPrimaryId == item.id )
            item.setProperty( "isSuperPrimary", true )
                .setProperty( "isPrimary", true )
          else if ( newSuperPrimaryId != null )
            item.setProperty( "isSuperPrimary", false )
          else
            item
        ))
      }
  }
}
