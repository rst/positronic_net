package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.util._
import org.positronicnet.facility._

import android.widget.{Spinner, LinearLayout}
import android.view.{View, ViewGroup, LayoutInflater, KeyEvent}
import android.app.Dialog

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

// Widget to display and update a TypeField, as above.

class TypeFieldChooser( ctx: Context, attrs: AttributeSet )
  extends PositronicSpinner( ctx, attrs )
{
  private var typeField: TypeField = null

  private val adapter = new IndexedSeqAdapter[String](
    IndexedSeq.empty, R.layout.simple_spinner_item)

  lazy val editCustomDialog = new EditCustomTypeDialog( this )

  setAdapter( adapter )
                                            
  def setTypeField( tf: TypeField ) = { 

    // Kludginess here --- we want to set the selection, but that
    // triggers the 'onItemSelected' below.  If the typefield was
    // already at the custom setting, we *don't* yet want the
    // 'onItemSelected' to pop up the edit dialog.  So we temporarily
    // set 'typeField' to null to disable it.

    typeField = null
    adapter.resetSeq( tf.displayStrings )
    setSelection( tf.selectedStringIdx, false )
    typeField = tf
  }

  def getTypeField = typeField

  onItemSelected{ (view, posn, id) =>
    if (typeField == null) {
      // still setting up; do nothing
    }
    else if (posn == typeField.info.customTypeIdx)
      editCustomDialog.doEditLabel( typeField )
    else {
      typeField = (typeField.recType_:=( typeField.info.recTypes( posn )))
      adapter.resetSeq( typeField.displayStrings )
    }
  }

  def setCustom( s: String ) = {
    typeField = typeField.label_:=( s )
    setTypeField( typeField )           // reinitialize display
  }

  def cancelCustom = setSelection( typeField.selectedStringIdx, false )
}

class EditCustomTypeDialog( typeFieldChooser: TypeFieldChooser )
  extends Dialog( typeFieldChooser.getContext )
  with TypedViewHolder 
{
  setContentView( R.layout.edit_custom_type_dialog )
  setTitle( R.string.enter_custom_label )

  val editTxt = findView( TR.dialogEditText )
  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }

  findView( TR.cancelButton ).onClick { doCancel; dismiss }
  findView( TR.saveButton ).onClick { doSave; dismiss }

  def doSave   = typeFieldChooser.setCustom( editTxt.getText.toString )
  def doCancel = typeFieldChooser.cancelCustom

  def doEditLabel( tf: TypeField ) = { 
    if (tf.label != null) 
      editTxt.setText( tf.label )
    show
  }
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

