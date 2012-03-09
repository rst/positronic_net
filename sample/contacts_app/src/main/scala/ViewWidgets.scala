package org.positronicnet.sample.contacts

import org.positronicnet.ui._

import android.content.Context
import android.util.AttributeSet

import android.view.{View, LayoutInflater}
import android.widget.LinearLayout

object ViewContactUiBinder extends UiBinder {

  bind[ CategoryDisplay, AggregatedDatum[_] ](
    (( categoryDisplay, datum ) => categoryDisplay.showForDatum( datum )),
    (( categoryDisplay, datum ) => datum ) // no update
    )

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
