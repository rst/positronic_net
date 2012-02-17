package org.positronicnet.sample.contacts

import org.positronicnet.orm.{RecordId, BatchScopeAction}
import org.positronicnet.orm.Actions._
import org.positronicnet.notifications.Actions._
import org.positronicnet.content.PositronicContentResolver

import scala.collection.mutable.{ArrayBuffer, HashMap}

import android.provider.ContactsContract
import android.provider.ContactsContract.{CommonDataKinds => CDK}
import android.util.Log

// Classes that implement the "business logic" of dealing with
// contacts, or at least the things we do with them.

// Class that represents an "edit state" for editing the ContactData
// items associated with a Contact or RawContact

class ContactEditState( val rawContact: RawContact,
                        initialItems: Seq[ ContactData ] ) 
  extends Serializable
{
  private var deletedState = new ArrayBuffer[ ContactData ]
  private var currentState = new ArrayBuffer[ ContactData ]
  private val accountInfo = AccountInfo.forRawContact( rawContact )

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
      batch.add( Save( item ))

    batch
  }

  // Determine what categories for a given type are available...
  // All of them, for the moment.

  def availableCategories( item: ContactData ) =
    item match {
      case typedItem: ContactDataWithCategoryLabel =>
        accountInfo.dataKinds.get( typedItem.typeTag ) match {
          case Some( kindInfo ) => kindInfo.categories
          case None => IndexedSeq.empty
        }
      case _ => IndexedSeq.empty
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

// Class that represents the value of a "label-or-custom" field.
// These are backed by two underlying fields, one an integer named
// "type" (which we generally style "tag" since "type" is a reserved
// word in Scala), and one the custom label, if any.

case class CategoryLabel( val tag: Int,  val label: String )
{
  def tag_:=( newTag: Int ) = this.copy( tag = newTag ) 
  def label_:=( s: String ) = this.copy( label = s )
}

// Information on account types...

object AccountInfo {

  def forRawContact( rawc: RawContact ) =
    rawc.accountType match {
      case "com.google" => 
        new GoogleAccountInfo( rawc.accountType, rawc.accountName )
      case _ => 
        new OtherAccountInfo( rawc.accountType, rawc.accountName )
    }
}

class DataKindInfo ( val categoryTagToResource: (Int => Int) = (x => -1),
                     val maxRecords: Int = -1 )
{
  private var categoriesBuf = new ArrayBuffer[ CategoryInfo ]

  lazy val categories: IndexedSeq[ CategoryInfo ] = categoriesBuf
  lazy val infoForCategoryTag = 
    Map( categories.map { x => (x.tag -> x) }: _* )

  def categoryTagToString( categoryTag: Int ): String =
    try {
      return Res.ources.getString( categoryTagToResource( categoryTag ))
    }
    catch {
      case _: Throwable =>
        return "Unknown type " + categoryTag
    }

  def categoryLabelToString( label: CategoryLabel ) =
    if (infoForCategoryTag( label.tag ).isCustom)
      label.label
    else
      categoryTagToString( label.tag )

  protected
  def category( categoryTag: Int, 
                maxRecord: Int = -1,
                isCustom: Boolean = false ) =
    categoriesBuf += CategoryInfo( this, categoryTag, maxRecords, isCustom )
}

case class CategoryInfo ( dataKindInfo: DataKindInfo,
                          tag: Int, 
                          maxRecords: Int, 
                          isCustom: Boolean )
{
  lazy val displayString = dataKindInfo.categoryTagToString( tag )
}

abstract class AccountInfo extends Serializable {
  def initialGroups: Seq[ Group ]
  val dataKinds: Map[ String, DataKindInfo ]
}

class OtherAccountInfo( acctType: String, acctName: String ) 
  extends AccountInfo
{
  def initialGroups = Seq.empty
  val dataKinds = 
    Map( 
      CDK.StructuredName.CONTENT_ITEM_TYPE -> 
        new DataKindInfo(),

      CDK.Phone.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.Phone.getTypeLabelResource _ ) {
          category( CDK.Phone.TYPE_HOME )
          category( CDK.Phone.TYPE_WORK )
          category( CDK.Phone.TYPE_MOBILE )
          category( CDK.Phone.TYPE_FAX_WORK )
          category( CDK.Phone.TYPE_FAX_HOME )
          category( CDK.Phone.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
          category( CDK.Phone.TYPE_CALLBACK )
          category( CDK.Phone.TYPE_CAR )
          category( CDK.Phone.TYPE_COMPANY_MAIN )
          category( CDK.Phone.TYPE_ISDN )
          category( CDK.Phone.TYPE_MAIN )
          category( CDK.Phone.TYPE_OTHER_FAX )
          category( CDK.Phone.TYPE_RADIO )
          category( CDK.Phone.TYPE_TELEX )
          category( CDK.Phone.TYPE_TTY_TDD )
          category( CDK.Phone.TYPE_WORK_MOBILE )
          category( CDK.Phone.TYPE_WORK_PAGER )
          category( CDK.Phone.TYPE_ASSISTANT )
          category( CDK.Phone.TYPE_MMS )
        },

      CDK.Email.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.Email.getTypeLabelResource _ ) {
          category( CDK.Email.TYPE_HOME )
          category( CDK.Email.TYPE_WORK )
          category( CDK.Email.TYPE_MOBILE )
          category( CDK.Email.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        }
    )
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

  val dataKinds = 
    Map( 
      CDK.StructuredName.CONTENT_ITEM_TYPE -> 
        new DataKindInfo,

      CDK.Phone.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.Phone.getTypeLabelResource _ ) {
          category( CDK.Phone.TYPE_HOME )
          category( CDK.Phone.TYPE_WORK )
          category( CDK.Phone.TYPE_MOBILE )
          category( CDK.Phone.TYPE_FAX_WORK )
          category( CDK.Phone.TYPE_FAX_HOME )
          category( CDK.Phone.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        },

      CDK.Email.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.Email.getTypeLabelResource _ ) {
          category( CDK.Email.TYPE_HOME )
          category( CDK.Email.TYPE_WORK )
          category( CDK.Email.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        }
    )
}

