package org.positronicnet.ui

import _root_.android.content.Context
import _root_.android.util.AttributeSet
import _root_.android.view.ViewGroup
import _root_.android.view.View
import _root_.android.view.Menu
import _root_.android.view.ContextMenu
import _root_.android.view.MenuItem
import _root_.android.widget.{AdapterView, SeekBar}
import _root_.android.util.Log
import _root_.android.view.KeyEvent
import _root_.android.view.View.OnKeyListener

import scala.collection.mutable.ArrayBuffer

/** Mixin trait for view subclasses which provides 
  * "JQuery-style" event listener declarations.  Fortunately, these
  * don't conflict with the native API because they're alternate
  * overloadings.
  *
  * NB the Scaladoc here is more readable if inherited methods
  * are omitted.
  */

trait PositronicHandlers extends View with GenericViewUtils {

  def setOnClickListener( dummy: View.OnClickListener ): Unit

  /** Call `func` when there is a click on this view. */

  def onClick(func: => Unit) = {
    setOnClickListener( new View.OnClickListener {
      def onClick( dummy: View ) = { func }
    })
  }

  def setOnKeyListener( dummy: OnKeyListener ): Unit

  private var genericOnKeyHandler: ((Int, KeyEvent) => Boolean) = null
  private var keyCodeHandlers: Map[ (Int,Int), (()=>Unit) ] = Map.empty
  private var haveInstalledKeyListener = false

  private [positronicnet]
  def installOnKeyListener = {
    if (!haveInstalledKeyListener) {
      val target = this
      setOnKeyListener( new OnKeyListener {
        def onKey( v: View, code: Int, ev: KeyEvent ) = 
          target.positronicKeyDispatch( code, ev.getMetaState, ev.getAction, ev)
      })
      haveInstalledKeyListener = true
    }
  }

  // Our dispatch logic, exposed here for the sake of tests.
  // (At time of writing, key dispatch stuff is awkward with Robolectric.)

  private [positronicnet]
  def positronicKeyDispatch( keyCode: Int, metaState: Int, 
                             action: Int, ev: KeyEvent ): Boolean = {
    if (action == KeyEvent.ACTION_DOWN) {
      for (handler <- keyCodeHandlers.get((keyCode, metaState))) {
        handler()
        return true
      }
    }
    if (genericOnKeyHandler != null) {
      return genericOnKeyHandler( keyCode, ev )
    }
    return false
  }

  /** Call the given `func` when this view has keyboard focus, and any
    * key is hit.  Arguments to `func` are the integer key code, and the
    * key event.  The `func` should return `true` if it handled the event,
    * `false` otherwise.
    *
    * Handlers declared for specific keycodes with `onKey( code ){ ... }`
    * take precedence, and will always override the generic handler here.
    */

  def onKey(func: ( Int, android.view.KeyEvent ) => Boolean):Unit = {
    installOnKeyListener
    genericOnKeyHandler = func
  }

  /** Call the given `func` on a keydown event (`KeyEvent.ACTION_DOWN`)
    * with the given keycode, and the given set of meta-modifiers.
    *
    * Useful to declare special handling for, e.g., `KeyEvent.KEYCODE_ENTER`.
    */

  def onKey(keyCode: Int, metaState: Int = 0)( func: => Unit ):Unit = {
    installOnKeyListener
    keyCodeHandlers += ((keyCode, metaState) -> (() => func))
  }

  /** Return this views most recent ancestor view of the given type, if any */

  def parentOfType[ ViewType <: View : ClassManifest ]: ViewType = 
    parentOfType[ ViewType ]( this )

  /** Return all children (or more distant descendents) of the given type
    * as an IndexedSeq
    */

  def childrenOfType[ ViewType <: View : ClassManifest ]: IndexedSeq[ViewType] =
    childrenOfType[ ViewType ]( this )

  /** Create a pop-up chooser with title text from the string resource
    * `titleRes`, and choice strings obtained by applying the `labeler`
    * to the given `vals`.  If the user makes a choice, the corresponding
    * `val` will be passed to the `handler`.
    */

  def withChoiceFromDialog[T](titleRes: Int, 
                              vals: IndexedSeq[T], 
                              labeler: T => String)
                             (handler: T => Unit): Unit = 
    withChoiceFromDialogInContext( this.getContext, titleRes, 
                                   vals, labeler )( handler )
}

/** "JQuery-style" handler declarations for TextView-specific conditions. */

trait PositronicTextViewHandlers extends android.widget.TextView {

  private val parentTextView = this

  /** Whenever the text changes, call `func` with the new string.
    *
    * Extreme shorthand for a common case of using TextWatchers.
    */

  def onTextChanged( func: String => Unit ) =
    glueTextWatcher.watchers += func

  private lazy val glueTextWatcher = makeGlueTextWatcher

  private def makeGlueTextWatcher = {

    val textWatcher = new android.text.TextWatcher {

      val watchers = new ArrayBuffer[ String => Unit ]

      def beforeTextChanged( s: CharSequence, start: Int, 
                             count: Int, after: Int ) = 
        ()

      def onTextChanged( s: CharSequence, start: Int, 
                         count: Int, after: Int ) = 
        ()

      def afterTextChanged( s: android.text.Editable ) = {
        val txt = parentTextView.getText.toString
        for ( watcher <- watchers )
          watcher( txt )
      }
    }

    this.addTextChangedListener( textWatcher )

    textWatcher
  }
}

/** "JQuery-style" handler declarations for AdapterView-specific events. */

trait PositronicItemHandlers {

  def setOnItemClickListener( l: AdapterView.OnItemClickListener ): Unit
  def setOnItemLongClickListener( l: AdapterView.OnItemLongClickListener ): Unit

  /** Call `func` when there is a click on an item.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.  The handler can call
    * `getItemAtPosition` to get the item itself, if need be.
    */

  def onItemClick( func: (View, Int, Long) => Unit) = {
    setOnItemClickListener( new AdapterView.OnItemClickListener {
      def onItemClick( parent: AdapterView[_], view: View,
                       position: Int, id: Long ) = { 
        func( view, position, id ) 
      }
    })
  }
   
  /** Call `func` when there is a long click on an item.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.  The handler can call
    * `getItemAtPosition` to get the item itself, if need be.
    *
    * The framework is always told that the event was handled; use
    * `onItemLongClickMaybe` if your handler might want to decline
    * the event, and leave it to the framework.
    */

  def onItemLongClick( func: (View, Int, Long) => Unit) = {
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id ); return true
      }
    })
  }
   
  /** Call `func` when there is a long click on an item.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.
    *
    * Return value is `true` if the event was handled; `false` otherwise.
    */

  def onItemLongClickMaybe( func:(View, Int, Long) => Boolean)={
    setOnItemLongClickListener( new AdapterView.OnItemLongClickListener {
      def onItemLongClick( parent: AdapterView[_], 
                           view: View,
                           position: Int, id: Long ):Boolean = { 
        func( view, position, id )
      }
    })
  }

  def getItemAtPosition( posn: Int ):Object

  def setOnItemSelectedListener( l: AdapterView.OnItemSelectedListener ):Unit

  // The item selected listener stuff is particularly awkward, since
  // we've got two distinct event handlers bundled up in one listener
  // class...

  private var haveOnItemSelectedListener = false
  private var itemSelectedHandler:    (( View, Int, Long ) => Unit) = null
  private var nothingSelectedHandler: (View => Unit)                = null

  private def installOnItemSelectedListener = {
    if (!haveOnItemSelectedListener) {

      haveOnItemSelectedListener = true

      setOnItemSelectedListener( new AdapterView.OnItemSelectedListener {

        def onItemSelected( parent: AdapterView[_], view: View, 
                            position: Int, id: Long ) = {
          handleItemSelected( view, position, id )
        }

        def onNothingSelected( parent: AdapterView[_] ) = {
          handleNothingSelected
        }

      })
    }
  }

  /** Call `func` when an item is selected.  Arguments to 
    * `func` are the `View` that was hit, its position, and the
    * `id` of the corresponding item.  The handler can call
    * `getItemAtPosition` to get the item itself, if need be.
    */

  def onItemSelected( handler: (View, Int, Long) => Unit ) = {
    installOnItemSelectedListener
    itemSelectedHandler = handler
  }

  /** Call `func` when the selection vanishes from this view ---
    * either because "touch is activated" (quoth the platform docs)
    * or because the adapter is now empty.
    */

  def onNothingSelected( handler: View => Unit ) = {
    installOnItemSelectedListener
    nothingSelectedHandler = handler
  }

  private def handleItemSelected( view: View, posn: Int, id: Long ) =
    if (itemSelectedHandler != null) itemSelectedHandler( view, posn, id )

  private def handleNothingSelected =
    if (nothingSelectedHandler != null) nothingSelectedHandler

  /** Return the item relevant to the `ContextMenu` selection which gave
    * us this `ContextMenuInfo`.
    */

  def selectedContextMenuItem( info: ContextMenu.ContextMenuInfo ):Object = {
    val posn = info.asInstanceOf[ AdapterView.AdapterContextMenuInfo ].position
    return getItemAtPosition( posn )
  }
   
}

// Widgets (and other things) with our traits premixed in.
//
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
// I'm not sure how to access from code.
//
// The only obvious workaround would be to have Foo and StyledFoo
// variants, with the StyledFoo having the three-arg constructor.
// But since the guts of everything is in traits, that's not hard.

/** An `android.widget.Button` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicButton( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Button( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.Checkbox with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicCheckBox( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.CheckBox( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.SeekBar with onProgressChange convenience method
  * à la [[org.positronicnet.ui.PositronicHandlers]]'s onClick etc
  */

class PositronicSeekBar( context : Context, attrs : AttributeSet = null )
  extends _root_.android.widget.SeekBar( context, attrs ) 
{
  private var progressChanged : (Int, Boolean) => Unit = null
  private var startTracking : () => Unit = null
  private var stopTracking : () => Unit = null

  def onProgressChanged( func : (Int, Boolean) => Unit ) = {
    progressChanged = func
    updateListener
  }
  
  def onStartTrackingTouch( func : () => Unit ) = {
    startTracking = func
    updateListener
  }

  def onStopTrackingTouch( func : () => Unit ) = {
    stopTracking = func
    updateListener
  }

  private def updateListener = {
    setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener {
 
      def onProgressChanged(sb : SeekBar, progress : Int, fromUser : Boolean) =
        if (progressChanged != null) progressChanged(progress, fromUser)
 
      def onStartTrackingTouch(sb : SeekBar) = 
        if (startTracking != null) startTracking
 
      def onStopTrackingTouch(sb : SeekBar) = 
        if (stopTracking != null) stopTracking
    })
  }
}

/** An `android.widget.ImageButton` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicImageButton( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.ImageButton( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.EditText` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicEditText( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.EditText( context, attrs ) 
 with PositronicHandlers
 with PositronicTextViewHandlers

/** An `android.widget.TextView` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicTextView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.TextView( context, attrs ) 
 with PositronicHandlers
 with PositronicTextViewHandlers

/** An `android.widget.ListView` with [[org.positronicnet.ui.PositronicHandlers]]
  * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
  */

class PositronicListView( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.ListView( context, attrs ) 
 with PositronicHandlers 
 with PositronicItemHandlers

/** An `android.widget.GridView` with [[org.positronicnet.ui.PositronicHandlers]]
 * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
 */

class PositronicGridView( context: Context, attrs: AttributeSet = null )
  extends _root_.android.widget.GridView( context, attrs )
  with PositronicHandlers
  with PositronicItemHandlers

/** An `android.widget.ExpandableListView` with [[org.positronicnet.ui.PositronicHandlers]]
 * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
 */

class PositronicExpandableListView( context: Context, attrs: AttributeSet = null )
  extends _root_.android.widget.ExpandableListView( context, attrs )
  with PositronicHandlers
  with PositronicItemHandlers

/** An `android.widget.GridView` with [[org.positronicnet.ui.PositronicHandlers]]
 * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
 */

class PositronicGridView( context: Context, attrs: AttributeSet = null )
        extends _root_.android.widget.GridView( context, attrs )
        with PositronicHandlers
        with PositronicItemHandlers

/** An `android.widget.Spinner` with [[org.positronicnet.ui.PositronicHandlers]]
  * and [[org.positronicnet.ui.PositronicItemHandlers]] mixed in.
  */

class PositronicSpinner( context: Context, attrs: AttributeSet = null )
 extends _root_.android.widget.Spinner( context, attrs ) 
 with PositronicHandlers 
 with PositronicItemHandlers

/** An `android.widget.ImageView` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicImageView( context: Context, attrs: AttributeSet = null )
 extends android.widget.ImageView( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.LinearLayout` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicLinearLayout( context: Context, attrs: AttributeSet = null )
 extends android.widget.LinearLayout( context, attrs ) 
 with PositronicHandlers

/** An `android.widget.RelativeLayout` with [[org.positronicnet.ui.PositronicHandlers]]
  * mixed in.
  */

class PositronicRelativeLayout( context: Context, attrs: AttributeSet = null )
 extends android.widget.RelativeLayout( context, attrs )
 with PositronicHandlers

/** Shorthand `android.app.Dialog` class with some extra constructor
  * arguments as a convenience.
  */

class PositronicDialog( context: Context, 
                        theme: Int = 0, 
                        layoutResourceId: Int = 0 )
 extends android.app.Dialog( context, theme ) 
{
  if ( layoutResourceId != 0 )
    setContentView( layoutResourceId )
}
