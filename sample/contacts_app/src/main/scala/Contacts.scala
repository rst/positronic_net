package org.positronicnet.sample.contacts

import org.positronicnet.content._
import org.positronicnet.orm._
import org.positronicnet.ui._
import org.positronicnet.notifications._
import org.positronicnet.notifications.Actions._
import org.positronicnet.facility._

import org.positronicnet.util.ReflectiveProperties
import org.positronicnet.util.ReflectUtils

import android.content.Context
import android.util.AttributeSet

import android.util.Log
import android.view.View
import android.view.KeyEvent
import android.widget.TextView

import android.provider.{ContactsContract => CC}
import android.provider.ContactsContract.CommonDataKinds

// Utility plumbing for dealing with resources.
// The naming here (Res.ources) is about as awkward as the mechanism...

object Res extends AppFacility {

  private var resCache: android.content.res.Resources = null

  protected override def realOpen( ctx: Context ): Unit = 
    resCache = ctx.getResources

  def ources = resCache
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
//
// (As are other details.  MS Exchange accounts are allowed only one
// mobile phone number, two home or work numbers, etc.; I guess you
// can ask Microsoft for the reasons.  And so forth.  What's supposed
// to happen when you create a contact with two mobile numbers and
// then assign it to an Exchange account is a bit mysterious...)

case class TypeFieldInfo(
  val labelTypes: Seq[Int],
  val customType: Int,
  val toResource: (Int => Int)
)

case class TypeField(
  val recType: Int,
  val label:   String,
  val info:    TypeFieldInfo
)
{
  def recType_:=( newType: Int ) = 
    this.copy( recType = newType, label = null )

  def label_:=( s: String ) = 
    this.copy( recType = info.customType, label = s )

  def isCustom = (recType == info.customType)

  def displayString =
    if (recType == info.customType)
      label
    else
      Res.ources.getString( info.toResource( recType ))
}

// Convenience class for digging as much as we can out of
// Android standard data...

class ReflectiveTypeFieldInfo[ Stuff : ClassManifest ]( 
    func: (Int => Int),
    types: Seq[String]
  )
  extends TypeFieldInfo( types.map{ ReflectUtils.getStatic[ Int, Stuff ](_) },
                         ReflectUtils.getStatic[ Int, Stuff ]( "TYPE_CUSTOM" ),
                         func )

// Contacts table.  For now, we're treating the whole thing as
// read-only, so we don't bother with read-only marks on particular
// columns.  (There are only a few modifiable columns; it might be
// worth some API to set the default, so we can special-case only the
// writable ones.)

case class Contact (
  val lookupKey:          String            = "",
  val primaryDisplayName: String            = "",
  val photoUri:           String            = "",
  val photoThumbnailUri:  String            = "",
  val inVisibleGroup:     Boolean           = false,
  val starred:            Boolean           = false,
  val customRingtone:     String            = "",
  val sendToVoicemail:    Boolean           = true,
  val id:                 RecordId[Contact] = Contacts.unsavedId
) 
extends ManagedRecord
{
  lazy val raw = 
    new HasMany( RawContacts, 
                 ReflectUtils.getStatic[ String, CC.RawContacts ]("CONTACT_ID"))
  lazy val data = 
    new HasMany( ContactData, 
                 ReflectUtils.getStatic[ String, CC.Data ]("CONTACT_ID"))

}

object Contacts
  extends RecordManagerForFields[ Contact, CC.Contacts ]

// Raw-contacts table.

case class RawContact (
  val contactId:       RecordId[Contact]    = Contacts.unsavedId,
  val starred:         Boolean              = false,
  val customRingtone:  String               = "",
  val sendToVoicemail: Boolean              = true,
  val accountName:     String               = "",
  val accountType:     String               = "",
  val id:              RecordId[RawContact] = RawContacts.unsavedId
) 
extends ManagedRecord
{
  lazy val data = 
    new HasMany( ContactData, 
                 ReflectUtils.getStatic[ String, CC.Data ]("RAW_CONTACT_ID"))
}

object RawContacts
  extends RecordManagerForFields[ RawContact, CC.RawContacts ]
{
  // Column names are inherited from protected static classes where
  // they're defined, and Scala has trouble fishing them out.  So,
  // we do this to get them by reflection at runtime. 

  private val col = ReflectUtils.getStatics[ String, CC.RawContacts ]

  // Most columns we're mapping are read/write.  The ID is read-only 
  // as usual, and we have these additional exceptions:

  mapField( "contactId",   col("CONTACT_ID"),   MapAs.ReadOnly  )
  mapField( "accountName", col("ACCOUNT_NAME"), MapAs.WriteOnce )
  mapField( "accountType", col("ACCOUNT_TYPE"), MapAs.WriteOnce )
}

// Contact-data table.  
//
// Note that "mimetype" isn't here because it isn't actually mapped
// for any subtype other than the catch-all; instead, we treat it as a
// discriminant that the variant-record machinery handles on its own.

abstract class ContactData 
  extends ManagedRecord with ReflectiveProperties
{
  // Generic fields...

  val contactId:      RecordId[ Contact ]    = Contacts.unsavedId
  val rawContactId:   RecordId[ RawContact ] = RawContacts.unsavedId
  val isPrimary:      Boolean                = false
  val isSuperPrimary: Boolean                = false
  val dataVersion:    Int                    = -1 // read-only
}

abstract class ContactDataWithRecordType extends ContactData 
{
  val recType: Int = PhoneTypeInfo.customType
  val label: String = "unset"

  val recTypeInfo: TypeFieldInfo

  lazy val recordType = TypeField( recType, label, recTypeInfo )

  def recordType_:=( newType: TypeField ) =
    if ( newType.recType == recTypeInfo.customType )
      this.setProperty[Int]("recType", newType.recType)
          .setProperty[String]("label", newType.label)
    else
      this.setProperty[Int]("recType", newType.recType)
          .setProperty[String]("label", null)

  def displayType = recordType.displayString
}

// Phone records.  Here's where we have a subset of what's really allowed
// (where what's allowed depends further on account type...)

class Phone extends ContactDataWithRecordType {
  val number:  String            = null
  val id:      RecordId[ Phone ] = ContactData.phones.unsavedId

  val recTypeInfo = PhoneTypeInfo
}

object PhoneTypeInfo 
  extends ReflectiveTypeFieldInfo[ CommonDataKinds.Phone ](
    (CommonDataKinds.Phone.getTypeLabelResource _),
    Seq( "TYPE_HOME", "TYPE_WORK", "TYPE_MOBILE", "TYPE_OTHER", "TYPE_CUSTOM" ))

// Email records.  Here we have the complete documented set, though
// not the Exchange restriction of a limit of three.

class Email extends ContactDataWithRecordType {
  val address: String = null
  val id:      RecordId[ Email ] = ContactData.emails.unsavedId

  val recTypeInfo = EmailTypeInfo
}

object EmailTypeInfo 
  extends ReflectiveTypeFieldInfo[ CommonDataKinds.Email ](
    (CommonDataKinds.Phone.getTypeLabelResource _),
    Seq( "TYPE_HOME", "TYPE_WORK", "TYPE_MOBILE", "TYPE_OTHER", "TYPE_CUSTOM" ))

// Unknown data records.  There's actually a defined way for third-party
// apps to specify how to display these, which is undocumented, and changed
// in a major way with ICS...

class UnknownData extends ContactData {
  val mimetype: String = null
  val data1:    String = null
  val id:       RecordId[ UnknownData ] = ContactData.unknowns.unsavedId
}

// Record mapper for the whole shebang.

object ContactData
  extends VariantRecordManager[ ContactData ](
    PositronicContentResolver( CC.Data.CONTENT_URI ),
    "mimetype" // documented value of ContactsContract.DataColumns.MIMETYPE
  )
{
  def dataKindMapper[ TRec <: ContactDataWithRecordType : ClassManifest,
                      TKind : ClassManifest ] = 
    new TaggedVariantForFields[ TRec, TKind ](
      ReflectUtils.getStatic[ String, TKind ]("CONTENT_ITEM_TYPE")
    ) {
      mapField( "recType", ReflectUtils.getStatic[ String, TKind ]("TYPE") ) 
      mapField( "dataVersion", 
                ReflectUtils.getStatic[ String, CC.Data ]("DATA_VERSION"),
                MapAs.ReadOnly )
    }

  val phones = dataKindMapper[ Phone, CommonDataKinds.Phone ] 
  val emails = dataKindMapper[ Email, CommonDataKinds.Email ] 

  val unknowns = 
    new CatchAllVariantForFields[ UnknownData, CC.Data ] {
      mapField( "dataVersion", 
                ReflectUtils.getStatic[ String, CC.Data ]("DATA_VERSION"),
                MapAs.ReadOnly )
    }
}

object ContactsUiBinder extends UiBinder

class ContactsActivity extends PositronicActivity
{
  onCreate {
    useAppFacility( PositronicContentResolver )
    useAppFacility( Res )               // stash a copy of the Resources
    setContentView( R.layout.contacts )

    Contacts.onThread {
      for ( contact <- Contacts.fetchOnThisThread ) {
        Log.d( "XXX", "Contact: " + contact.primaryDisplayName )
        for ( datum <- contact.data.fetchOnThisThread ) {
          datum match {
            case phone: Phone => 
              Log.d("XXX","  Phone: "+ phone.displayType +" "+ phone.number)
            case email: Email =>
              Log.d("XXX","  Email: "+ email.displayType +" "+ email.address)
            case stuff: UnknownData =>
              Log.d("XXX","  " + stuff.mimetype + " " + stuff.data1)
          }
        }
      }
    }
  }
}

