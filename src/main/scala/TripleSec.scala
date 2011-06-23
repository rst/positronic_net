package org.triplesec

import _root_.android.content.Context
import _root_.android.util.AttributeSet

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
