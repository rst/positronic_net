package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.util._
import org.positronicnet.facility._

import android.widget.{Spinner, LinearLayout, TextView, Button, Toast}
import android.view.{View, ViewGroup, LayoutInflater, KeyEvent}
import android.app.{Activity, Dialog}
import android.text.TextUtils

import android.content.Context
import android.util.{AttributeSet, Log}

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
  def bindState( state: ContactEditState ) = {

    for (editor <- childrenOfType[ DataKindEditor ](findView( TR.editors )))
      editor.bind( state )

    // Dealing with the "add section" widgetry.  The list of available
    // sections is computed dynamically off whichever are hidden (with
    // visibility set to View.GONE), after they've had a chance to
    // initialize themselves (and ensure they're visible if pre-populated).

    setupAddSectionWidget

    findView( TR.add_section ).onClick {

      val choices = this.hiddenDataKindEditors
      val title = R.string.add_section

      withChoiceFromDialog[ DataKindEditor ]( title, choices, _.sectionName ){
        dataKindEditor => {

          // Reposition the chosen DataKindEditor just above the choice bar
          // (so it's close by if the user wants to add something else)
          // and make it visible.  Then reset the list of what's available...
        
          val editors = findView( TR.editors )

          editors.removeView( dataKindEditor ) // ... from wherever it is
          editors.addView( dataKindEditor )    // ... at end
          dataKindEditor.setVisibility( View.VISIBLE )
          setupAddSectionWidget
        }
      }
    }
  }

  def updateState = 
    for (editor <- childrenOfType[ DataKindEditor ](findView( TR.editors )))
      editor.updateState

  def setupAddSectionWidget = {
    val hiddenEditors = this.hiddenDataKindEditors
    val resources = getContext.getResources
    val chooseString = resources.getString( R.string.sec_choose )
    val addString = resources.getString( R.string.sec_add )
    hiddenEditors.size match {
      case 0 =>
        findView( TR.section_add_row ).setVisibility( View.GONE )
      case 1 =>
        findView( TR.add_section ).setText( 
          addString + " " + hiddenEditors(0).sectionName )
      case 2 =>
        findView( TR.add_section ).setText(
          chooseString + " " +
          hiddenEditors(0).sectionName + ", " + hiddenEditors(1).sectionName )
      case _ =>
        findView( TR.add_section ).setText(
          chooseString + " " +
          hiddenEditors(0).sectionName + ", " + hiddenEditors(1).sectionName +
          ", ...")
    }
  }

  def hiddenDataKindEditors = {
    val allEditors = childrenOfType[ DataKindEditor ](findView( TR.editors ))
    allEditors.filter{ _.getVisibility == View.GONE }
  }
}

// Widget to display all ContactData of a particular "kind" (Phone, Email, etc.)

class DataKindEditor( ctx: Context, attrs: AttributeSet )
  extends LinearLayout( ctx, attrs )
  with WidgetUtils
  with TypedViewHolder
{
  var state: ContactEditState = null    // really set at bind()

  val inflater = 
    ctx.getSystemService( Context.LAYOUT_INFLATER_SERVICE )
      .asInstanceOf[ LayoutInflater ]

  val itemLayoutResId = attrs.getAttributeResourceValue( null, "itemLayout", 0 )
  val targetKlass = Class.forName( attrs.getAttributeValue( null, "class" ))

  lazy val sectionName = findView( TR.section_header ).getText.toString

  if (itemLayoutResId == 0)
    throw new RuntimeException( "No item layout specified for " +
                                this.toString + " in XML" )

  val rawItemBuilder = ReflectUtils.getObjectBuilderForClass( targetKlass )
  val itemBuilder = rawItemBuilder.asInstanceOf[ () => ContactData ]

  // Hooks for our enclosing RawContactEditor, to manage startup and save

  def bind( state: ContactEditState ) = {

    this.state = state
    val ourData = state.currentItems.filter( targetKlass.isInstance( _ ))

    if (ourData.isEmpty) {
      // No initial data; prepare a starter item for later use
      addDatumEditor
    }
    else {
      setVisibility( View.VISIBLE )   // have data; make sure user sees it!
      for ( item <- ourData ) 
        newView.bind( item )
    }
  }

  def updateState =
    for (cde <- childrenOfType[ ContactDatumEditor ]( this ))
      state.updateItem( cde.updatedItem )

  // Hooks for our subsidiary add- and remove-item buttons

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

  // Creating a view to manage a single item. 

  def newView = {
    val v = inflater.inflate( itemLayoutResId, this, false )
    addView( v )
    v.asInstanceOf[ ContactDatumEditor ] // it better be!
  }
}

// Widgets coordinating editing of a single ContactData item, of
// whatever type.  (All LinearLayouts for now, but we can mix
// the trait into other stuff if need be.)

trait ContactDatumEditor extends WidgetUtils with TypedViewHolder {

  var item: ContactData = null

  def bind ( item: ContactData ) = {
    this.item = item
    ContactsUiBinder.show( item, this )
  }

  def updatedItem = ContactsUiBinder.update( this.item, this )
  
  def doDelete = parentOfType[ DataKindEditor ].killDatumEditor( this )
}

class ContactDatumEditLayout( ctx: Context, attrs: AttributeSet )
  extends LinearLayout( ctx, attrs )
  with ContactDatumEditor

// Some of these have "detail" fields that we don't show until
// the user asks for them explicitly

trait HiddenContactDataFields extends ContactDatumEditor {

  def handleExpCollapseButton( button: Button,
                               showStringRes: Int, 
                               hideStringRes: Int,
                               controlledIds: Set[ Int ] ) = 
  {
    val res = getContext.getResources
    val showString = res.getString( showStringRes )
    val hideString = res.getString( hideStringRes )

    if (button.getText.toString == showString) {
      button.setText( hideString )
      for (textView <- this.childrenOfType[ TextView ])
        if (controlledIds.contains( textView.getId ))
          textView.setVisibility( View.VISIBLE )
    }
    else {
      button.setText( showString )
      for (textView <- this.childrenOfType[ TextView ])
        if (controlledIds.contains( textView.getId ) &&
            !TextUtils.isGraphic( textView.getText ))
          textView.setVisibility( View.GONE )
    }
  }
}

// Special-case behavior for structured name ContactDatumEditor

class StructuredNameEditLayout( ctx: Context, attrs: AttributeSet )
  extends ContactDatumEditLayout( ctx, attrs )
  with HiddenContactDataFields
{
  // Our "hide/show" buttons start in "hide" state, so faking clicks hides the
  // unpopulated fields...

  override def bind ( item: ContactData ) = {
    super.bind( item )
    detailClick
    phoneticClick
    findView( TR.detailButton ).onClick { detailClick }
    findView( TR.phoneticButton ).onClick { phoneticClick }
  }

  // Mechanics of "hide/show" buttons

  def detailClick = 
    handleExpCollapseButton( findView( TR.detailButton ), 
                             R.string.show_detail, R.string.hide_detail,
                             Set( R.id.prefix, R.id.middleName, R.id.suffix ))

  def phoneticClick = 
    handleExpCollapseButton( findView( TR.phoneticButton ), 
                             R.string.show_phonetic, R.string.hide_phonetic,
                             Set( R.id.phoneticGivenName, 
                                  R.id.phoneticMiddleName, 
                                  R.id.phoneticFamilyName ))
}

// Special-case behavior for postal address ContactDatumEditor

class PostalEditLayout( ctx: Context, attrs: AttributeSet )
  extends ContactDatumEditLayout( ctx, attrs )
  with HiddenContactDataFields
{
  override def bind ( item: ContactData ) = {
    super.bind( item )
    detailClick
    findView( TR.detailButton ).onClick{ detailClick }
  }

  def detailClick =
    handleExpCollapseButton( findView( TR.detailButton ),
                             R.string.show_detail, R.string.hide_detail,
                             Set( R.id.pobox,
                                  R.id.neighborhood,
                                  R.id.country ))
 }

// Widgets for "Add" and "Remove" buttons for ContactDataEditors.

class AddItemButton( ctx: Context, attrs: AttributeSet ) 
  extends PositronicButton(ctx, attrs) with WidgetUtils 
{
  onClick { parentOfType[ DataKindEditor ].addDatumEditor }
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
  lazy val editState   = parentOfType[ DataKindEditor ].state
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
          editCustomDialog.doEdit( categoryLabel.tag_:=( category.tag ))
        else
          setCategoryLabel( categoryLabel.tag_:=( category.tag ).label_:=(null))
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
  var categoryLabel: CategoryLabel = null

  setContentView( R.layout.edit_custom_type_dialog )
  setTitle( R.string.enter_custom_label )

  val editTxt = findView( TR.dialogEditText )
  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }

  findView( TR.cancelButton ).onClick { dismiss }
  findView( TR.saveButton ).onClick { doSave; dismiss }

  def doSave = categoryChooser.setCategoryLabel( 
    this.categoryLabel.label_:=( editTxt.getText.toString ))

  def doEdit( label: CategoryLabel ) = { 
    this.categoryLabel = label
    if (label.label != null)
      editTxt.setText( label.label )
    show
  }
}

