package org.positronicnet.sample.contacts

import org.positronicnet.content._
import org.positronicnet.orm._
import org.positronicnet.notifications._
import org.positronicnet.notifications.Actions._

import org.positronicnet.util.ReflectiveProperties
import org.positronicnet.util.ReflectUtils

import android.content.Context
import android.util.AttributeSet

import android.provider.{ContactsContract => CC}
import android.provider.ContactsContract.CommonDataKinds

// Convenience class for digging as much as we can out of
// Android standard data...

class ReflectiveTypeFieldInfo[ Stuff : ClassManifest ]( 
    func: (Int => Int),
    types: IndexedSeq[String]
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
  val displayNamePrimary: String            = "",
  val photoUri:           String            = "",
  val photoThumbnailUri:  String            = "",
  val inVisibleGroup:     Boolean           = false,
  val starred:            Boolean           = false,
  val customRingtone:     String            = "",
  val sendToVoicemail:    Boolean           = true,
  val id:                 RecordId[Contact] = Contacts.unsavedId
) 
extends ManagedRecord with ReflectiveProperties
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
//
// Actual individual contacts, associated with accounts, which the
// provider aggregates into the globs that are presented to users.

case class RawContact (
  val contactId:       RecordId[Contact]    = Contacts.unsavedId,
  val starred:         Boolean              = false,
  val customRingtone:  String               = "",
  val sendToVoicemail: Boolean              = true,
  val deleted:         Boolean              = false,
  val accountName:     String               = "",
  val accountType:     String               = "",
  val id:              RecordId[RawContact] = RawContacts.unsavedId
) 
extends ManagedRecord with ReflectiveProperties
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

// Groups table.
//
// Groups right now are per account.  We're mapping a subset...

class Group extends ManagedRecord {

  val accountName:  String          = null
  val accountType:  String          = null
  val title:        String          = null
  val notes:        String          = null
  val groupVisible: Boolean         = false
  val id:           RecordId[Group] = Groups.unsavedId

  override def toString = "Group: " + 
    (if (!groupVisible) "[INVIS] " else "") +
    accountName + " " + accountType + " '" +
    title + "' '" + notes + "'"
}

object Groups extends RecordManagerForFields[ Group, CC.Groups ]
{
  private val col = ReflectUtils.getStatics[ String, CC.Groups ]

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

  override def toString = 
    super.toString + " id: " + id + " rcid: " + rawContactId
}

// Structured name records.  Note the special-case treatment of columns
// on insert/update.  If we submit a display name *and nothing else*, the
// content provider will attempt to fill in the first name, last name, etc.,
// based on splitting the display name.  However, submitting even null or
// empty values will prevent that.  So, we rig the record manager to produce
// a mix most likely to get the behavior we want out of the content provider
// in given instances.

class StructuredName extends ContactData
{
  val displayName: String = null

  val prefix:     String = null
  val givenName:  String = null
  val middleName: String = null
  val familyName: String = null
  val suffix:     String = null

  val phoneticGivenName:  String = null
  val phoneticMiddleName: String = null
  val phoneticFamilyName: String = null

  val id: RecordId[ StructuredName ] = ContactData.structuredNames.unsavedId

  override def toString = {
    val components =
      Seq( displayName, prefix, givenName, middleName, familyName, suffix )
    super.toString + " '" + components.reduceLeft(_+"' '"+_) + "'"
  }
}

trait StructuredNameManager[T <: StructuredName] extends BaseRecordManager[T] {

  override def dataPairs( n: T ) = {

    import CommonDataKinds.{StructuredName => SN}

    val basePairs = super.dataPairs(n)

    val snFields = Seq( 
      SN.PREFIX, SN.SUFFIX,
      SN.GIVEN_NAME, SN.MIDDLE_NAME, SN.FAMILY_NAME,
      SN.PHONETIC_GIVEN_NAME, SN.PHONETIC_MIDDLE_NAME, SN.PHONETIC_FAMILY_NAME)

    val snPairs = basePairs.filter{ p => snFields.contains( p._1 )}

    if (snPairs.exists{ _._2 != org.positronicnet.content.CvString(null) })
      basePairs
    else
      basePairs.filter{ p => !snFields.contains( p._1 )}
  }

}

// Group membership records.  For inserts, you can set a "source id"
// as opposed to the actual group ID; we're not bothering with this
// for now.

class GroupMembership extends ContactData
{
  val groupRowId: RecordId[Group] = Groups.unsavedId
  val id: RecordId[GroupMembership] = ContactData.groupMemberships.unsavedId

  override def toString = super.toString + " group id: " + groupRowId
}

// Common machinery for rows that have a "record type", which is
// jargon for a Home/Work/Mobile category, as for phone numbers
// or email addresses.
//
// Note that we are not yet dealing with restrictions on Exchange
// account contacts, which are limited both in the total number of,
// say, email addresses associated with any one contact, and the
// number of email addresses of a particular type ("Home", "Work",
// etc.)
//
// (FWIW, support for these restrictions is why the interface has to
// ask what account that you're adding a contact to before adding any
// data records --- it needs to know which set of restrictions applies,
// and that depends on the account type.)

abstract class ContactDataWithRecordType extends ContactData 
{
  val recType: Int    = PhoneTypeInfo.customType
  val label:   String = null

  val recTypeInfo: TypeFieldInfo

  def recordType = TypeField( recType, label, recTypeInfo )

  def recordType_:=( newType: TypeField ) =
    if ( newType.recType == recTypeInfo.customType )
      this.setProperty[Int]("recType", newType.recType)
          .setProperty[String]("label", newType.label)
    else
      this.setProperty[Int]("recType", newType.recType)
          .setProperty[String]("label", null)

  def displayType = recordType.displayString
}

// Phone records.  Here's where we now have a subset of what's really allowed
// (where what's allowed depends further on account type...)

class Phone extends ContactDataWithRecordType {
  val number:  String            = null
  val id:      RecordId[ Phone ] = ContactData.phones.unsavedId

  val recTypeInfo = PhoneTypeInfo

  override def toString = 
    super.toString + " ("+ recordType.displayString +", "+ number +")"
}

object PhoneTypeInfo 
  extends ReflectiveTypeFieldInfo[ CommonDataKinds.Phone ](
    (CommonDataKinds.Phone.getTypeLabelResource _),
    IndexedSeq( "TYPE_HOME", "TYPE_WORK", "TYPE_MOBILE", 
                "TYPE_OTHER", "TYPE_CUSTOM" ))

// Email records.  Here we have the complete documented set, though
// not the Exchange restriction of a limit of three.

class Email extends ContactDataWithRecordType {
  val address: String = null
  val id:      RecordId[ Email ] = ContactData.emails.unsavedId

  val recTypeInfo = EmailTypeInfo

  override def toString = 
    super.toString + " ("+ recordType.displayString +", "+ address+")"
}

object EmailTypeInfo 
  extends ReflectiveTypeFieldInfo[ CommonDataKinds.Email ](
    (CommonDataKinds.Phone.getTypeLabelResource _),
    IndexedSeq( "TYPE_HOME", "TYPE_WORK", "TYPE_MOBILE", 
                "TYPE_OTHER", "TYPE_CUSTOM" ))

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
  val CONTACT_ID = ReflectUtils.getStatic[ String, CC.Data ]("CONTACT_ID")

  class DataKindMapper[ TRec <: ContactData : ClassManifest,
                        TKind : ClassManifest ]
    extends TaggedVariantForFields[ TRec, TKind ](
      ReflectUtils.getStatic[ String, TKind ]("CONTENT_ITEM_TYPE")
    ) 
  {
    mapField( "contactId", CONTACT_ID, MapAs.ReadOnly )
    mapField( "dataVersion", 
              ReflectUtils.getStatic[ String, CC.Data ]("DATA_VERSION"),
              MapAs.ReadOnly )
  }

  class TypedDataKindMapper[ TRec <: ContactDataWithRecordType : ClassManifest,
                             TKind : ClassManifest ]
    extends TaggedVariantForFields[ TRec, TKind ](
      ReflectUtils.getStatic[ String, TKind ]("CONTENT_ITEM_TYPE")
    ) 
    {
      mapField( "contactId", CONTACT_ID, MapAs.ReadOnly )
      mapField( "recType", ReflectUtils.getStatic[ String, TKind ]("TYPE") ) 
      mapField( "dataVersion", 
                ReflectUtils.getStatic[ String, CC.Data ]("DATA_VERSION"),
                MapAs.ReadOnly )
    }

  val phones = new TypedDataKindMapper[ Phone, CommonDataKinds.Phone ] 
  val emails = new TypedDataKindMapper[ Email, CommonDataKinds.Email ] 

  val groupMemberships =
    new DataKindMapper[ GroupMembership, CommonDataKinds.GroupMembership ]
  val structuredNames = 
    new DataKindMapper[ StructuredName, CommonDataKinds.StructuredName ]
      with StructuredNameManager[ StructuredName ]

  val unknowns = 
    new CatchAllVariantForFields[ UnknownData, CC.Data ] {
      mapField( "dataVersion", 
                ReflectUtils.getStatic[ String, CC.Data ]("DATA_VERSION"),
                MapAs.ReadOnly )
    }
}


