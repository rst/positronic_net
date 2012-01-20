package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.util._
import org.positronicnet.facility._

import android.widget.{Spinner, LinearLayout}
import android.view.{View, ViewGroup, LayoutInflater}

import android.content.Context
import android.util.{AttributeSet, Log}

// Utility class for binding widgets to data items.  Standard
// facilities plus a few extra...

object ContactsUiBinder extends UiBinder {

  bindProperties[ TypeFieldChooser, TypeField ](
    (_.getTypeField), (_.setTypeField( _ )))
  
}

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

case class TypeFieldInfo(
  val recTypes: IndexedSeq[Int],
  val customType: Int,
  val toResource: (Int => Int)
)

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
    this.copy( recType = newType, label = null )

  def label_:=( s: String ) = 
    this.copy( recType = info.customType, label = s )

  def isCustom = {recType == info.customType}

  def displayString = displayStringOfRecType( recType )

  def displayStrings = recTypes.map{ displayStringOfRecType(_) }

  def selectedStringIdx = recTypes.indexOf( recType )

  def displayStringOfRecType( recType: Int ) =
    if (recType == info.customType)
      label
    else {
      val str = Res.ources.getString( info.toResource( recType ))
      if (str != null)
        str
      else
        "Unknown type " + recType
    }
}

// Widget to display and update a TypeField, as above.

class TypeFieldChooser( ctx: Context, attrs: AttributeSet )
  extends Spinner( ctx, attrs )
{
  // XXX popping up a dialog to set the custom value when it is selected.

  private var baseTypeField: TypeField = null

  private val adapter = new IndexedSeqAdapter[String](
    IndexedSeq.empty, R.layout.simple_spinner_item)

  setAdapter( adapter )
                                            
  def setTypeField( tf: TypeField ) = {
    baseTypeField = tf
    adapter.resetSeq( tf.displayStrings )
    setSelection( tf.selectedStringIdx, false )
  }

  def getTypeField = 
    baseTypeField.recType_:=( getSelectedItemPosition )
}

// Widget to display all ContactData of a particular type (Phone, Email, etc.)

abstract
class CategoryDisplay[ T <: ContactData : ClassManifest ]
  (ctx: Context, attrs: AttributeSet)
    extends LinearLayout( ctx, attrs )
{
  val inflater = 
    ctx.getSystemService( Context.LAYOUT_INFLATER_SERVICE )
      .asInstanceOf[ LayoutInflater ]

  val dataLayoutResId =  attrs.getAttributeResourceValue(null, "dataLayout", 0)

  val targetKlass = classManifest[T].erasure

  if (dataLayoutResId == 0)
    throw new RuntimeException( "No data layout specified for " +
                                this.toString + " in XML" )

  val builder = ReflectUtils.getObjectBuilder[T]
  def newItem = builder()

  def newView = {
    val v = inflater.inflate( dataLayoutResId, this, false )
    addView( v )
    v
  }

  def bind( stuff: Seq[ ContactData ] ) =
    for( item <- stuff ) 
      if (targetKlass.isInstance( item )) 
        ContactsUiBinder.show( item, newView )
}

class StructuredNameDisplay( ctx: Context, attrs: AttributeSet )
  extends CategoryDisplay[StructuredName]( ctx, attrs )

class PhoneDisplay( ctx: Context, attrs: AttributeSet )
  extends CategoryDisplay[ Phone ]( ctx, attrs )

class EmailDisplay( ctx: Context, attrs: AttributeSet )
  extends CategoryDisplay[ Email ]( ctx, attrs )

