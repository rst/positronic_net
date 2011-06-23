package org.triplesec

import _root_.android.content.Context
import _root_.android.util.AttributeSet
import _root_.android.view.LayoutInflater

trait DryerHandlers {

  // "JQuery-style" event listener declarations.  Fortunately, these
  // don't conflict with the native API because they're alternate
  // overloadings.

  def setOnClickListener( dummy: android.view.View.OnClickListener ): Unit

  def onClick(func: => Unit) = {
    setOnClickListener( new android.view.View.OnClickListener {
      def onClick( dummy: android.view.View ) = { func }
    })
  }

}

// Note that we don't (yet) provide the three-argument constructor
// variants of these, since we've got a bit of a catch-22 with it.
//
// Scala classes must have a single "main" constructor which all
// the others call --- they can't directly invoke overloaded
// constructors in the base class.  If we supported the three-arg
// variant (with "style" as the third argument), then the other
// two would have to supply the default value.  But the proper
// value to supply, from consulting the android source directly,
// is something fished out of com.android.internal.R which user 
// code doesn't have access to.
//
// The only obvious workaround would be to have Foo and StyledFoo
// variants, with the StyledFoo having the three-arg constructor.
// Which would require all the actual methods to be declared in
// traits to avoid duplication here; fortunately, that's not hard.

class Button( context: Context, attrs: AttributeSet )
 extends _root_.android.widget.Button( context, attrs ) with DryerHandlers {

   def this( context: Context ) = this( context, null )
}

// Adapters for Scala collections.  Also support an alternative
// API which DRYs up common invocation patterns.
//
// Note that right now, there's some ugliness due to the method
// "getItem(_:Int):Object" declared in the Adapter interface;
// that entails both the "T <: Object" type class restriction,
// and the getItemTyped silliness.

class IndexedSeqAdapter[T <: Object](seq: IndexedSeq[T], 
                                     itemViewResourceId: Int = 0, 
                                     itemTextResourceId: Int = 0
                                    ) 
extends _root_.android.widget.BaseAdapter {

  var inflater: LayoutInflater = null

  def getView( position: Int, 
               convertView: android.view.View,
               parent: android.view.ViewGroup ): android.view.View = {

    val view = 
      if (convertView != null) {
        convertView
      }
      else {
        if (inflater == null) {
          inflater = 
            parent.getContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE)
             .asInstanceOf[LayoutInflater]
        }
        createView( parent )
      }

    fillView( view, position )
    return view
  }

  def createView(parent: android.view.ViewGroup): android.view.View = {
    assert( itemViewResourceId != 0 )
    inflater.inflate( itemViewResourceId, parent, false )
  }

  def fillView( view: android.view.View, position: Int ) = {
    val textView = 
      (if (itemTextResourceId != 0)
        view.findViewById( itemTextResourceId )
       else
         view).asInstanceOf[ android.widget.TextView ]

    textView.setText( getItem( position ).toString )
  }

  // Accessors.  Note that getItem can't be declared to return a [T],
  // because that conflicts with the return type declared in Adapter.
  // Thus, getItemTyped.

  def getItem(position: Int):Object = seq(position)
  def getItemTyped(position: Int):T = seq(position)
  def getItemId(position: Int) = getItem(position).hashCode()
  def getCount = seq.size
}
