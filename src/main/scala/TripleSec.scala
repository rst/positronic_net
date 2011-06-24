package org.triplesec

import _root_.android.content.Context
import _root_.android.util.AttributeSet
import _root_.android.view.LayoutInflater
import _root_.android.os.Bundle
import _root_.android.widget.AdapterView

import rst.todo.TypedResource           // bletch! XXX TOFIX

// Getting sub-widgets, using the typed resources consed up by the
// android SBT plugin.

trait DryerViewOps {

  def findView[T](  tr: TypedResource[T] ) = 
    findViewById( tr.id ).asInstanceOf[T]

  def findViewById( id: Int ): android.view.View

}

// "JQuery-style" event listener declarations.  Fortunately, these
// don't conflict with the native API because they're alternate
// overloadings.

trait DryerHandlers extends DryerViewOps {

  def setOnClickListener( dummy: android.view.View.OnClickListener ): Unit

  def onClick(func: => Unit) = {
    setOnClickListener( new android.view.View.OnClickListener {
      def onClick( dummy: android.view.View ) = { func }
    })
  }

  def setOnKeyListener( dummy: android.view.View.OnKeyListener ): Unit

  def onKey(func: ( Int, android.view.KeyEvent ) => Boolean) = {
    setOnKeyListener( new android.view.View.OnKeyListener {
      def onKey( dummy: android.view.View, 
                 keyCode: Int, event: android.view.KeyEvent ):Boolean = {
        return func( keyCode, event )
      }
    })
  }

  // Handler for a *specific* key.  Arguable bug:  can only declare one!
  // Not hard to fix, but not needed for now.  Mark XXX TODO.

  def onKey(keyCode: Int, metaState: Int = 0)( func: => Unit ) = {
    setOnKeyListener( new android.view.View.OnKeyListener {
      def onKey( dummy: android.view.View, 
                 eventKeyCode: Int, event: android.view.KeyEvent ):Boolean = {
        if (eventKeyCode == keyCode && event.getMetaState == metaState) {
          func; return true
        } else {
          return false
        }
      }
    })
  }

}

// DRYer handlers for AdapterView-specific events.
//
// There is weirdness here; an onItemLongClick handler can return false
// to mark the event unhandled, but the onItemClick handler is defined
// to return Unit (a/k/a a Java void), and is just assumed to have handled
// the event.  What?!
//
// Our onItemLongClick is like onItemClick; it takes a handler which returns
// Unit, and always returns 'true' (event handled) to the framework.  If you
// really want the choice, there's onItemLongClick maybe, which takes a 
// handler returning a Boolean.  (We can't use overloading here, because
// the handler types are the same after erasure.)

trait DryerItemHandlers {

  def setOnItemClickListener( l: AdapterView.OnItemClickListener ): Unit
  def setOnItemLongClickListener( l: AdapterView.OnItemLongClickListener ): Unit

  def onItemClick( func: (_root_.android.view.View, Int, Long) => Unit) = {
    setOnItemClickListener( new AdapterView.OnItemClickListener {
      def onItemClick( parent: AdapterView[_], view: _root_.android.view.View,
                       position: Int, id: Long ) = { 
        func( view, position, id ) 
      }
    })
  }
   
  def onItemLongClick( func: (_root_.android.view.View, Int, Long) => Unit) = {
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: _root_.android.view.View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id ); return true
      }
    })
  }
   
  def onItemLongClickMaybe( func:(_root_.android.view.View, Int, Long) => Boolean)={
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: _root_.android.view.View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id )
      }
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

class Button( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Button( context, attrs ) with DryerHandlers

class EditText( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.EditText( context, attrs ) with DryerHandlers

class TextView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.TextView( context, attrs ) with DryerHandlers

class ListView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.ListView( context, attrs ) 
 with DryerHandlers 
 with DryerItemHandlers

class Dialog( context: Context, theme: Int = 0, layoutResourceId: Int = 0 )
 extends android.app.Dialog( context, theme ) with DryerViewOps {

  if ( layoutResourceId != 0 )
    setContentView( layoutResourceId )
}

class Activity( layoutResourceId: Int = 0 )
 extends android.app.Activity with DryerViewOps {

  // Handlers for lifecycle events.  The idea here is simply to
  // eliminate the ceremony of having to call super.foo() when
  // redefining each of these.
  //
  // Note also the variant handling of instance state --- the
  // explicit saveInstanceState( Bundle ) and recreateInstanceState( Bundle )
  // methods, called from the "on..." variant, or onCreate, respectively,
  // again to eliminate ceremony.

  var onCreateHandler:  ( () => Unit ) = null
  var onRestartHandler: ( () => Unit ) = null
  var onStartHandler:   ( () => Unit ) = null
  var onResumeHandler:  ( () => Unit ) = null
  var onPauseHandler:   ( () => Unit ) = null
  var onStopHandler:    ( () => Unit ) = null
  var onDestroyHandler: ( () => Unit ) = null

  override def onCreate( b: Bundle ) = {
    super.onCreate( b )
    if (layoutResourceId != 0) { setContentView( layoutResourceId ) }
    if (onCreateHandler != null) { onCreateHandler() }
    recreateInstanceState( b )
  }

  def onCreate( handler: => Unit ) = { onCreateHandler = ( () => handler ) }

  override def onRestart = { 
    super.onRestart(); 
    if (onRestartHandler != null) { onRestartHandler() }
  }
  
  def onRestart( handler: => Unit ) = { onRestartHandler = ( () => handler ) }

  override def onResume = { 
    super.onResume(); 
    if (onResumeHandler != null) { onResumeHandler() }
  }
  
  def onResume( handler: => Unit ) = { onResumeHandler = ( () => handler ) }

  override def onPause = { 
    super.onPause(); 
    if (onPauseHandler != null) { onPauseHandler() }
  }
  
  def onPause( handler: => Unit ) = { onPauseHandler = ( () => handler ) }

  override def onStop = { 
    super.onStop(); 
    if (onStopHandler != null) { onStopHandler() }
  }
  
  def onStop( handler: => Unit ) = { onStopHandler = ( () => handler ) }

  override def onDestroy = { 
    super.onDestroy(); 
    if (onDestroyHandler != null) { onDestroyHandler() }
  }
  
  def onDestroy( handler: => Unit ) = { onDestroyHandler = ( () => handler ) }

  def saveInstanceState( b: Bundle ) = {}
  def recreateInstanceState( b: Bundle ) = {}
  def restoreInstanceState( b: Bundle ) = {}

  override def onSaveInstanceState( b: Bundle ) = {
    super.onSaveInstanceState( b )
    saveInstanceState( b )
  }

  override def onRestoreInstanceState( b: Bundle ) = {
    super.onSaveInstanceState( b )
    saveInstanceState( b )
  }

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
