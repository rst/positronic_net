package org.positronicnet.sample.contacts

import org.positronicnet.ui._

import android.content.{Context, Intent}
import android.util.{AttributeSet, Log}

import android.view.{View, LayoutInflater}
import android.widget.{LinearLayout, ImageView}
import android.net.Uri

object ViewContactUiBinder extends UiBinder {

  bind[ CategoryDisplay, AggregatedDatum[_] ](
    (( categoryDisplay, datum ) => categoryDisplay.showForDatum( datum )),
    (( categoryDisplay, datum ) => datum ) // no update
    )

  bind[ PhoneWidget, Phone ](
    ( _.bindItem( _ )),
    ( (display, datum) => datum))

  bind[ EmailWidget, Email ](
    ( _.bindItem( _ )),
    ( (display, datum) => datum))

}

class ContactDataAggregateDisplay( ctx: Context, attrs: AttributeSet )
  extends LinearLayout( ctx, attrs )
  with TypedViewHolder
  with WidgetUtils
{
  def bind( data: AggregatedData ) =
    for (display <- childrenOfType[ DataKindDisplay ])
      display.bind( data )
}

class DataKindDisplay( ctx: Context, attrs: AttributeSet )
  extends LinearLayout( ctx, attrs )
  with TypedViewHolder
  with WidgetUtils
{
  val inflater = 
    ctx.getSystemService( Context.LAYOUT_INFLATER_SERVICE )
      .asInstanceOf[ LayoutInflater ]

  val itemLayoutResId = attrs.getAttributeResourceValue( null, "itemLayout", 0 )
  val targetKlassUnk = Class.forName( attrs.getAttributeValue( null, "class" ))
  val targetKlass = 
    targetKlassUnk.asInstanceOf[ Class[T] forSome {type T <: ContactData} ]

  def bind( allData: AggregatedData ) = {
    val myData = allData.dataOfClass( targetKlass )
    if (!myData.isEmpty) {
      setVisibility( View.VISIBLE )
      for (aggregatedDatum <- myData) {

        val view = inflater.inflate( itemLayoutResId, this, false )
        addView( view )

        // We have an AggregatedDatum which pairs a raw ContactData
        // object with the accountInfo of the account it came from.
        // Some widgets are associated with string properties of the
        // raw datum; others need the AccountInfo to interpret.  So,
        // we have the binder try it both ways...

        ViewContactUiBinder.show( aggregatedDatum, view )
        ViewContactUiBinder.show( aggregatedDatum.datum, view )
      }
    }
  }
}

class CategoryDisplay( ctx: Context, attrs: AttributeSet )
  extends PositronicTextView( ctx, attrs )
{
  def showForDatum( agg: AggregatedDatum[_] ) =
    agg.datum match {
      case datum: ContactDataWithCategoryLabel =>
        agg.acctInfo.dataKinds.get( datum.typeTag ).map { info =>
          setText( info.categoryLabelToString( datum.categoryLabel )) }
      case _ =>
    }
}

// A lot of our widgets, when clicked, start an activity relevant in
// some way to the contents of some ContactData row.  This trait abstracts
// the basic pattern...

trait ActivityStarterFor[ Item <: ContactData ] 
  extends WidgetUtils
  with ActivityResultDispatchClient
  with PositronicHandlers
{
  var item: Item
  def activityResultDispatchKey = item.id
  def bindItem( item: Item ) = { 
    Log.d( "XXX", "Bind " + item.toString )
    this.item = item
    onClick { startActivity( makeIntent ) }
  }
  def makeIntent: Intent
}

abstract class ActivityStartingGroup[ Item <: ContactData ]( 
    ctx: Context,
    attrs: AttributeSet ) 
  extends LinearLayout( ctx, attrs )
  with ActivityStarterFor[ Item ]
  with UiBindingsForSelfAndChildren

// Details...

class PhoneWidget( ctx: Context, attrs: AttributeSet )
  extends ActivityStartingGroup[ Phone ]( ctx, attrs )
{
  var item: Phone = null
  def makeIntent =
    new Intent( Intent.ACTION_CALL, Uri.parse( "tel:" + this.item.number ))
}

class SmsImage( ctx: Context, attrs: AttributeSet )
  extends ImageView( ctx, attrs )
  with ActivityStarterFor[ Phone ]
{
  var item: Phone = null
  def makeIntent =
    new Intent( Intent.ACTION_VIEW, Uri.parse( "smsto:" + this.item.number ))
}

class EmailWidget( ctx: Context, attrs: AttributeSet )
  extends ActivityStartingGroup[ Email ]( ctx, attrs )
{
  var item: Email = null
  def makeIntent =
    new Intent( Intent.ACTION_SENDTO, Uri.parse( "mailto:" + this.item.address))
}

class WebsiteWidget( ctx: Context, attrs: AttributeSet )
  extends ActivityStartingGroup[ Website ]( ctx, attrs )
{
  var item: Website = null
  def makeIntent =
    new Intent( Intent.ACTION_VIEW, Uri.parse( this.item.url ))
}
