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

  val rawContacts =
    for ((rawc, data) <- rawData)
    yield rawc

  // Aggregated data for this contact, with an attempt to eliminate
  // duplicates, suitable for populating an inspector ...

  def aggregatedData = {

    val aggregate = new AggregatedData
    
    for ( rawState <- this.rawContactEditStates )
      for ( item <- rawState.currentItems )
        if (! item.isEmpty )
          aggregate.addItem( AggregatedDatum( item, rawState.accountInfo ))
        else
          println( "empty: " + item )

    aggregate
  }

  // Mark a given datum "superprimary" for its data type --- i.e.,
  // the one to use if you're using only one for the whole aggregate
  // contact.

  def markSuperPrimary( datum: ContactData ) = 
    newSuperPrimary( datum.getClass ) = datum.id

  private val newSuperPrimary = new HashMap[ Class[_], RecordId[_] ]

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

// Class which provides a snapshot of a group of data, typically from
// an aggregated contact, regardless of source, merging "lookalike"
// records that are duplicated across more than one contact.

class AggregatedData {

  private val aggregators = new HashMap[ Class[_], ItemAggregator[_] ]

  // Return all aggregated data of the given type

  def dataOfType[ Item <: ContactData : ClassManifest ] = {
    val klass = classManifest[ Item ].erasure.asInstanceOf[ Class[Item] ]
    dataOfClass[ Item ]( klass )
  }

  // Return all aggregated data of the given class.

  def dataOfClass[Item <: ContactData]( klass: Class[Item] ) 
       :Seq[ AggregatedDatum [ Item ]] = 
  {
    aggregators.get( klass ) match {
      case Some( aggregator ) =>
        aggregator.items.asInstanceOf[ Seq[ AggregatedDatum[ Item ]]]
      case None => Seq.empty
    }
  }

  def addItem[ Item <: ContactData]( item: AggregatedDatum[ Item ]) = {
    val klass = item.datum.getClass

    aggregators.get( klass ) match {

      case Some( aggregator ) => 
        aggregator.asInstanceOf[ ItemAggregator[ Item ]].add( item )

      case None =>
        aggregators( klass ) = item.datum match {
          case it: ContactDataWithCategoryLabel =>
            newCategorizedAggregator( 
              item.asInstanceOf[ AggregatedDatum[ 
                                   _ <: ContactDataWithCategoryLabel]] )
          case _ =>
             newAggregator( item )
        }
    }
  }

  private
  class ItemAggregator[ Item <: ContactData ] {

    val data = new HashMap[ AnyRef, AggregatedDatum [ Item ]]

    def add( item: AggregatedDatum[ Item ] ): this.type = {
      val key = item.datum.equivalenceKey
      data.get( key ) match {
        case None => 
          data( key ) = item
        case Some( other ) =>
          data( key ) = chooseItem( item, data( key ))
      }
      this
    }

    // Have two "substantially similar" items with (i.e., same key) 
    // --- need to pick one.  Default: choose arbitrarily.

    def chooseItem( item: AggregatedDatum[ Item ], 
                    item2: AggregatedDatum[ Item ] ) = 
      item

    def items: IndexedSeq[ AggregatedDatum[ Item ]] = 
      data.values.toIndexedSeq
  }
  
  private
  class CategorizedItemAggregator[ Item <: ContactDataWithCategoryLabel ]
    extends ItemAggregator[ Item ]
  {
    // Have two items which are "substantially similar", but may have
    // different category labels.  We pick the one with whose category
    // tag has the lowest numeric value; this gives custom labels 
    // priority, and otherwise is as sensible as anything else we
    // might do.

    override def chooseItem( item:  AggregatedDatum[ Item ], 
                             item2: AggregatedDatum[ Item ] ) = 
      if (item.datum.categoryTag < item2.datum.categoryTag)
        item
      else
        item2

    override def items = 
      data.values.toIndexedSeq.sortBy{ (item: AggregatedDatum[ Item ]) => 
        item.datum.categoryTag }
  }

  private
  def newAggregator[ Item <: ContactData ]( item: AggregatedDatum[ Item ] ) = 
    new ItemAggregator[ Item ].add( item )

  private
  def newCategorizedAggregator [ T <: ContactDataWithCategoryLabel]( 
      item: AggregatedDatum[ T ] )= 
    new CategorizedItemAggregator[ T ].add( item )
}

case class AggregatedDatum[ T <: ContactData ]( 
  datum: T, 
  acctInfo: AccountInfo 
)
