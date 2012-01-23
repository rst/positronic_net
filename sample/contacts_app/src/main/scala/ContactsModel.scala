package org.positronicnet.sample.contacts

import org.positronicnet.orm.RecordId
import scala.collection.mutable.{ArrayBuffer, HashMap}

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

  def logIt = {

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

