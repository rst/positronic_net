package org.positronicnet.sample.contacts

import scala.collection.mutable.ArrayBuffer

import android.provider.ContactsContract
import android.provider.ContactsContract.{CommonDataKinds => CDK}

import org.positronicnet.notifications.Actions._
import org.positronicnet.notifications.Future

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

abstract class AccountInfo extends Serializable {
  val initialGroupQuery: Future[ IndexedSeq[ Group ]]
  val dataKinds: Map[ String, DataKindInfo ]
}

class OtherAccountInfo( acctType: String, acctName: String ) 
  extends AccountInfo
{
  val initialGroupQuery = Future[ IndexedSeq[ Group ]]( IndexedSeq.empty )
  val dataKinds = BaseAccountInfo.dataKinds
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

object BaseAccountInfo {
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
        },

      CDK.StructuredPostal.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.StructuredPostal.getTypeLabelResource _ ) {
          category( CDK.StructuredPostal.TYPE_HOME )
          category( CDK.StructuredPostal.TYPE_WORK )
          category( CDK.StructuredPostal.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        },

      CDK.Organization.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.Organization.getTypeLabelResource _ ) {
          category( CDK.Organization.TYPE_WORK )
          category( CDK.Organization.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        },

      CDK.Im.CONTENT_ITEM_TYPE ->
        new DataKindInfo( CDK.Im.getProtocolLabelResource _ ) {
          category( CDK.Im.PROTOCOL_AIM )
          category( CDK.Im.PROTOCOL_MSN )
          category( CDK.Im.PROTOCOL_YAHOO )
          category( CDK.Im.PROTOCOL_SKYPE )
          category( CDK.Im.PROTOCOL_QQ )
          category( CDK.Im.PROTOCOL_GOOGLE_TALK )
          category( CDK.Im.PROTOCOL_ICQ )
          category( CDK.Im.PROTOCOL_JABBER )
          category( CDK.Im.PROTOCOL_CUSTOM, isCustom = true )
        },

      CDK.Website.CONTENT_ITEM_TYPE  -> new DataKindInfo(),
      CDK.Note.CONTENT_ITEM_TYPE     -> new DataKindInfo(),
      CDK.Nickname.CONTENT_ITEM_TYPE -> new DataKindInfo()
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

  // Try to find our default group, in the background

  val initialGroupScope = Groups.whereEq( "accountName" -> acctName,
                                          "accountType" -> acctType,
                                          "title" -> myContactsName ) 

  val initialGroupQuery = initialGroupScope ? Query

  val dataKinds = 
    BaseAccountInfo.dataKinds
     .updated( CDK.Phone.CONTENT_ITEM_TYPE,
        new DataKindInfo( CDK.Phone.getTypeLabelResource _ ) {
          category( CDK.Phone.TYPE_HOME )
          category( CDK.Phone.TYPE_WORK )
          category( CDK.Phone.TYPE_MOBILE )
          category( CDK.Phone.TYPE_FAX_WORK )
          category( CDK.Phone.TYPE_FAX_HOME )
          category( CDK.Phone.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        })
     .updated( CDK.Email.CONTENT_ITEM_TYPE,
        new DataKindInfo( CDK.Email.getTypeLabelResource _ ) {
          category( CDK.Email.TYPE_HOME )
          category( CDK.Email.TYPE_WORK )
          category( CDK.Email.TYPE_OTHER )
          category( CDK.BaseTypes.TYPE_CUSTOM, isCustom = true )
        })
}

