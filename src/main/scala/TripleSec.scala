package org.triplesec

import _root_.android.content.Context
import _root_.android.util.AttributeSet
import _root_.android.view.LayoutInflater

import rst.todo.TypedResource           // bletch!

trait DryerViewOps {

  // Getting sub-widgets, using the typed resources consed up by the
  // android SBT plugin.

  def findView[T](  tr: TypedResource[T] ) = 
    findViewById( tr.id ).asInstanceOf[T]

  def findViewById( id: Int ): android.view.View

}

trait DryerHandlers extends DryerViewOps {

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

// Note that we don't (yet) provide allthe constructor variants
// with "theme" arguments, since we've got a bit of a catch-22 
// with them.
//
// Scala classes must have a single "main" constructor which all the
// others call --- they can't directly invoke overloaded constructors
// in the base class.  If we supported all of the "style"-as-arg
// variants (e.g., the three-argument constructors for "Button", etc.,
// with "style" as the third ag), then the other constructors would
// have to supply the default value.  But the proper value to supply,
// at least in the Button case, from consulting the android source
// directly, is something fished out of com.android.internal.R which
// user code doesn't have access to.
//
// The only obvious workaround would be to have Foo and StyledFoo
// variants, with the StyledFoo having the three-arg constructor.
// Which would require all the actual methods to be declared in
// traits to avoid duplication here; fortunately, that's not hard.

class Button( context: Context, attrs: AttributeSet )
 extends _root_.android.widget.Button( context, attrs ) with DryerHandlers {

   def this( context: Context ) = this( context, null )
}

class Dialog( context: Context, theme: Int = 0, layoutResourceId: Int = 0 )
extends android.app.Dialog( context, theme ) with DryerViewOps {

  if ( layoutResourceId != 0 )
    setContentView( layoutResourceId )

}

// Adapters for Scala collections.  Also support an alternative
// API which DRYs up common invocation patterns.
//
// Note that the "T <: Object" restriction is needed so that
// our "getItem( _: Int ):T" is compatible with the declared
// "getItem( _: Int ): java.lang.Object" in the Adapter interface.
// So, if you really want an adapter for an IndexedSeq[Long],
// you're on your own.

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

  // Accessors.  

  def getItem(position: Int):T = seq(position)
  def getItemId(position: Int) = getItem(position).hashCode()
  def getCount = seq.size
}
