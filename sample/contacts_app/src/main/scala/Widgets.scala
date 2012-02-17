package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.util._
import org.positronicnet.facility._

import android.widget.{Spinner, LinearLayout}
import android.view.{View, ViewGroup, LayoutInflater, KeyEvent}
import android.app.{Activity, Dialog}

import android.content.Context
import android.util.{AttributeSet, Log}
import android.widget.Toast

// Utility class for binding widgets to data items.  Standard
// facilities plus a few extra...

object ContactsUiBinder extends UiBinder {
  bindProperties[ CategoryChooser, CategoryLabel ](
    (_.getCategoryLabel), (_.setCategoryLabel( _ )))
}

// Utility plumbing for dealing with resources.
// The naming here (Res.ources) is about as awkward as the mechanism...

object Res extends AppFacility {

  private var resCache: android.content.res.Resources = null

  protected override def realOpen( ctx: Context ): Unit = 
    resCache = ctx.getResources

  def ources = resCache
}

// Widget encompassing all data display for editing a RawContact

class RawContactEditor( ctx: Context, attrs: AttributeSet ) 
  extends LinearLayout( ctx, attrs )
  with TypedViewHolder
  with WidgetUtils
{
  def bindState( state: ContactEditState ) = 
    for (editor <- childrenOfType[ DataKindEditor[_] ](findView( TR.editors )))
      editor.bind( state )

  def updateState = 
    for (editor <- childrenOfType[ DataKindEditor[_] ](findView( TR.editors )))
      editor.updateState
}

// Widget to display all ContactData of a particular type (Phone, Email, etc.)

abstract class DataKindEditor[ T <: ContactData : ClassManifest ]
  (ctx: Context, attrs: AttributeSet)
    extends LinearLayout( ctx, attrs )
    with WidgetUtils
{
  var state: ContactEditState = null    // really set at bind()

  val inflater = 
    ctx.getSystemService( Context.LAYOUT_INFLATER_SERVICE )
      .asInstanceOf[ LayoutInflater ]

  val dataLayoutResId = attrs.getAttributeResourceValue(null, "dataLayout", 0)
  val targetKlass = classManifest[T].erasure

  if (dataLayoutResId == 0)
    throw new RuntimeException( "No data layout specified for " +
                                this.toString + " in XML" )

  val itemBuilder = ReflectUtils.getObjectBuilder[T]

  def newView = {
    val v = inflater.inflate( dataLayoutResId, this, false )
    addView( v )
    v.asInstanceOf[ ContactDatumEditor ] // it better be!
  }

  def bind( state: ContactEditState ) = {
    this.state = state
    for ( item <- state.currentItems ) 
      if (targetKlass.isInstance( item ))
        newView.bind( item )
  }

  def updateState =
    for (cde <- childrenOfType[ ContactDatumEditor ]( this ))
      state.updateItem( cde.updatedItem )

  def addDatumEditor = 
    state.prepareForInsert( itemBuilder() ) match {

      case Some( newItem ) => 
        val view = newView
        view.bind( newItem )
        view.requestFocus

      case None => 
        Toast.makeText( getContext, R.string.no_more_items, Toast.LENGTH_LONG )
    }

  def killDatumEditor( child: ContactDatumEditor ) = {
    state.deleteItem( child.updatedItem )
    this.removeView( child )
  }
}

abstract class SingletonDataKindEditor[ T <: ContactData : ClassManifest ]
  (ctx: Context, attrs: AttributeSet)
    extends DataKindEditor[T]( ctx, attrs )
{
  // We expect one instance of our particular data type (though we show
  // more if we get them).  If we get none, we create a starter item.

  override def bind( state: ContactEditState ) = {
    super.bind( state )
    if (!state.currentItems.exists( targetKlass.isInstance( _ )))
      addDatumEditor
  }
}

class StructuredNameDisplay( ctx: Context, attrs: AttributeSet )
  extends SingletonDataKindEditor[StructuredName]( ctx, attrs )

class PhoneDisplay( ctx: Context, attrs: AttributeSet )
  extends DataKindEditor[ Phone ]( ctx, attrs )

class EmailDisplay( ctx: Context, attrs: AttributeSet )
  extends DataKindEditor[ Email ]( ctx, attrs )

// Widgets coordinating editing of a single ContactData item, of
// whatever type.  (All LinearLayouts for now, but we can mix
// the trait into other stuff if need be.)

trait ContactDatumEditor extends WidgetUtils {

  var item: ContactData = null

  def bind ( item: ContactData ) = {
    this.item = item
    ContactsUiBinder.show( item, this )
  }

  def updatedItem = ContactsUiBinder.update( this.item, this )
  
  def doDelete = parentOfType[ DataKindEditor[_] ].killDatumEditor( this )
}

class ContactDatumEditLayout( ctx: Context, attrs: AttributeSet )
  extends LinearLayout( ctx, attrs )
  with ContactDatumEditor

// Widgets for "Add" and "Remove" buttons for category items.

class AddItemButton( ctx: Context, attrs: AttributeSet ) 
  extends PositronicButton(ctx, attrs) with WidgetUtils 
{
  onClick { parentOfType[ DataKindEditor[_] ].addDatumEditor }
}

class RemoveItemButton( ctx: Context, attrs: AttributeSet ) 
  extends PositronicButton(ctx, attrs) with WidgetUtils 
{
  onClick { parentOfType[ ContactDatumEditor ].doDelete }
}

// Widget to display and update a CategoryLabel

class CategoryChooser( ctx: Context, attrs: AttributeSet )
  extends PositronicButton( ctx, attrs )
  with WidgetUtils
{
  // The category label that we're managing

  private var categoryLabel: CategoryLabel = null

  // Our metadata (mostly fished out of the ContactDatumEditor of which
  // we are effectively a component).

  lazy val datumEditor = parentOfType[ ContactDatumEditor ]
  lazy val editState   = parentOfType[ DataKindEditor[_] ].state
  lazy val info        = editState.dataKindInfo( datumEditor.item ).get

  // Hooks for the UiBinder

  def getCategoryLabel = this.categoryLabel
  def setCategoryLabel( categoryLabel: CategoryLabel ) = { 
    this.categoryLabel = categoryLabel
    setText( info.categoryLabelToString( categoryLabel ))
  }

  // User interaction

  lazy val editCustomDialog = new EditCustomCategoryDialog( this )

  onClick {

    val title   = R.string.choose_category
    val choices = editState.availableCategories( datumEditor.item )

    withChoiceFromDialog[ CategoryInfo ]( title, choices, _.displayString ){
      category => {
        if ( category.isCustom )
          editCustomDialog.doEdit( categoryLabel )
        else
          setCategoryLabel( categoryLabel.tag_:=( category.tag ) )
      }
    }
  }

  // Hook for EditCustomCategoryDialog...

  def setCustom( s: String ) = setCategoryLabel( categoryLabel.label_:=( s ))
}

class EditCustomCategoryDialog( categoryChooser: CategoryChooser )
  extends Dialog( categoryChooser.getContext )
  with TypedViewHolder 
{
  setContentView( R.layout.edit_custom_type_dialog )
  setTitle( R.string.enter_custom_label )

  val editTxt = findView( TR.dialogEditText )
  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }

  findView( TR.cancelButton ).onClick { dismiss }
  findView( TR.saveButton ).onClick { doSave; dismiss }

  def doSave = categoryChooser.setCustom( editTxt.getText.toString )

  def doEdit( label: CategoryLabel ) = { 
    if (label.label != null)
      editTxt.setText( label.label )
    show
  }
}

