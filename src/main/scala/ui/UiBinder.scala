package org.positronicnet.ui

import android.preference.{Preference,PreferenceGroup,
                           CheckBoxPreference,EditTextPreference}

import android.view.{View,ViewGroup}
import android.widget.{TextView,CheckBox}

import org.positronicnet.util.ReflectiveProperties
import org.positronicnet.util.PropertyLensFactory

// Feasibility study for binding UI components.
// May ultimately want to sugar the syntax here.
//
// (May also want to ultimately factor out commonalities with 
// code that walks a View hierarchy.)

private [ui]
abstract class UiBinding {
  def show (widget: Object, props: Object): Unit
  def update (widget: Object, props: Object): Object
}

private [ui]
class BindingManager {

  protected
  var bindingMap: Map[Class[_], UiBinding] = Map.empty

  protected
  def noteBinding( klass: Class[_], binder: UiBinding ) =
    bindingMap += ( klass -> binder )

  private [ui]
  def findBinder( obj: Object ) = {
    val klass = obj.getClass
    bindingMap.get( klass ) match {
      case Some(binding) => Some(binding)
      case None =>
        findBinderForSuperclass( klass.getSuperclass ) match {
          case Some(binding) => 
            bindingMap += (klass -> binding)
            Some( binding )
          case None =>
            None
        }
    }
  }

  private
  def findBinderForSuperclass( klass: Class[_] ): Option[UiBinding] = {
    var kklass = klass
    while( kklass != null ) {
      bindingMap.get( kklass ) match {
        case Some(binding) => return Some(binding)
        case None          => kklass = kklass.getSuperclass
      }
    }
    return None
  }
}

private [ui]
class SimpleBinding[ TWidget <: Object, TData <: Object ](
    showFunc: (TWidget, TData) => Unit,
    updateFunc: (TWidget, TData) => TData)
 extends UiBinding
{
  def show( widget: Object, data: Object ) =
    showFunc( widget.asInstanceOf[ TWidget ],
              data.asInstanceOf[ TData ] )
  
  def update( widget: Object, data: Object): Object =
    updateFunc( widget.asInstanceOf[ TWidget ], 
                data.asInstanceOf[ TData ])
}

private [ui]
class PropertyBinding[ TWidget, TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  )
  extends UiBinding
{
  // XXX This is ridiculous.  We know which branch of the match will
  // be taken at declare time; no need for reflection at all here!
  // At the very least, could have two subclasses with two different
  // implementations, and let 'PropertyBinder.declare' pick the right
  // one...

  def propertyName( widget: TWidget ) =
    widget match {
      case pref: android.preference.Preference => pref.getKey
      case view: android.view.View =>
        ResourceId.toName( view.getId ).getOrElse("")
    }

  val lensFactory = PropertyLensFactory.forPropertyType[ TProp ]
  val propKlass = classManifest[ TProp ].erasure

  def lens (widget: TWidget, props: ReflectiveProperties) = {
    val propsKlass = props.getClass.asInstanceOf[ Class[Object] ]
    val propName = propertyName( widget )
    assert( propName != null, "null property name for " + widget.toString )
    lensFactory.forProperty( propsKlass, propName ) match {
      case Some( lens ) => lens
      case None =>
        throw new RuntimeException( "Could not find property " + 
                                    propertyName( widget ) + " of type " +
                                    propKlass.toString + 
                                    " in class " + propsKlass.toString )
    }
  }

  def show (widget: Object, props: Object) = {
    val wwidget = widget.asInstanceOf[ TWidget ]
    val pprops = props.asInstanceOf[ ReflectiveProperties ]
    writeFunc( wwidget, lens( wwidget, pprops ).getter( pprops ) )
  }

  def update (widget: Object, props: Object) = {
    val wwidget = widget.asInstanceOf[ TWidget ]
    val pprops = props.asInstanceOf[ ReflectiveProperties ]
    val result = lens( wwidget, pprops ).setter( pprops, readFunc( wwidget ))
    result.asInstanceOf[ ReflectiveProperties ]
  }
}

private [ui]
class PropertyBinder extends BindingManager {

  def bindProperties[ TWidget : ClassManifest, TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  ) =
    noteBinding( classManifest[ TWidget ].erasure,
                 new PropertyBinding( readFunc, writeFunc ) )
}

/** Class that helps shuffle data between properties of
  * [[org.positronicnet.util.ReflectiveProperties]] objects and
  * Android user interface widgets.  Each widget has a particular
  * type of property that it can handle; a `CheckBoxPreference`
  * can handle `Boolean` properties, an `EditTextPreference` can
  * handle `String` properties, and so forth.
  *
  * Actual shuffling is done by calling the `show` and `update`
  * methods of a [[org.positronicnet.ui.UiBinder]], which was builg
  * with this [[org.positronicnet.ui.PropertyBinder]] as the
  * argument to its constructor.
  */

class UiBinder
  extends BindingManager 
{
  private val propertyBinder = new PropertyBinder

  // A few stock bindings --- checkbox prefs can set boolean properties;
  // EditTextPreferences can set String properties.  Subclasses can add more.

  bindProperties[ EditTextPreference, String ](
    (_.getText), (_.setText( _ )))

  bindProperties[ CheckBoxPreference, Boolean ](
    (_.isChecked), (_.setChecked( _ )))

  bindProperties[ TextView, String ](
    (_.getText.toString), (_.setText( _ )))

  bindProperties[ CheckBox, Boolean ](
    (_.isChecked), (_.setChecked( _ )))

  /** Declare that widgets of type `TWidget` can be used to render
    * or set properties of type `TProp`.  The caller must supply two
    * functions to manage the mechanics of the shuffling:
    * a `readFunc` to get a `TProp` out of a `TWidget`,
    * and a `writeFunc` to put a `TProp` into a `TWidget`.  
    *
    * `TWidget` must be a subclass of `android.preference.Preference`;
    * Views will be supported too, which is why the typechecker doesn't
    * enforce this.
    * 
    * The default behavior is specified as follows; declarations for
    * other preference types is specified similarly:
    * {{{
    *     bindProperties[ EditTextPreference, String ](
    *       (_.getText), (_.setText( _ )))
    *
    *     bindProperties[ CheckBoxPreference, Boolean ](
    *       (_.isChecked), (_.setChecked( _ )))
    *
    *     bindProperties[ TextView, String ](
    *       (_.getText.toString), (_.setText( _ )))
    *     
    *     bindProperties[ CheckBox, Boolean ](
    *       (_.isChecked), (_.setChecked( _ )))
    * }}}
    */

  def bindProperties[ TWidget : ClassManifest, TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  ) =
    propertyBinder.bindProperties[ TWidget, TProp ](readFunc, writeFunc)

  /** Declare that widgets of type `TWidget` can be used to directly
    * render or update objects of type `TData`.  The caller must supply two
    * functions to manage the mechanics of the shuffling:
    * a `showFunc` to update a `TWidget` from the state of a `TData`,
    * and an `updateFunc` to update a `TData` into a `TWidget`.  
    *
    * `TWidget` must be a subclass of `android.preference.Preference`;
    * Views will be supported too, which is why the typechecker doesn't
    * enforce this.
    */

  def bind[ TWidget <: Object : ClassManifest, TData <: Object ](
    showFunc: (TWidget, TData) => Unit,
    updateFunc: (TWidget, TData) => TData
  ) =
    noteBinding( classManifest[ TWidget ].erasure,
                 new SimpleBinding( showFunc, updateFunc ))

  private
  def getBinder( obj: Object ) =
    this.findBinder (obj) orElse propertyBinder.findBinder (obj)

  /** Update an Android `Preference` (or `PreferenceGroup`) based on
    * the properties of the object `toShow`.
    * 
    * A `PreferenceGroup` is handled by iterating over its members.  (If
    * those contain nested `PreferenceGroup`s, we iterate over their members
    * too.)  Otherwise, we proceed as follows:
    *
    * If a binder has been declared for the particular `Preference` type
    * and the class of `toShow` (q.v. `bind`), then it is used to handle
    * the data transfer.  Otherwise, if the `Preference` has been bound
    * to a particular property type with `bindProperties`, and if `toShow`
    * is a [[org.postronicnet.util.ReflectiveProperties]] object, we look
    * for a property of that type named by the `Preference`'s key
    * (viz. `getKey`).
    *
    * Otherwise, if we can't find a relevant declared UI Binding, a
    * [[org.positronicnet.ui.NoBinderFor]] exception is thrown at runtime.
    */

  def show( toShow: Object, pref: Preference ): Unit = {
    pref match {
      case grp: PreferenceGroup =>
        for (i <- 0 to grp.getPreferenceCount - 1)
          show( toShow, grp.getPreference( i ))
      case _ =>
        val binder = getBinder(pref).getOrElse(throw new NoBinderFor(pref))
        binder.show( pref, toShow )
    }
  }

  /** Use an Android `Preference` (or `PreferenceGroup`) to produce
    * an updated version of the [[org.positronicnet.util.ReflectiveProperties]]
    * object `toUpdate`.
    * 
    * A `PreferenceGroup` is handled by iterating over its members.  (If
    * those contain nested `PreferenceGroup`s, we iterate over their members
    * too.)  Otherwise, we proceed as follows:
    *
    * If a binder has been declared for the particular `Preference` type
    * and the class of `toShow` (q.v. `bind`), then it is used to handle
    * the data transfer.  Otherwise, if the `Preference` has been bound
    * to a particular property type with `bindProperties`, and if `toShow`
    * is a [[org.postronicnet.util.ReflectiveProperties]] object, we look
    * for a property of that type named by the `Preference`'s key
    * (viz. `getKey`).
    *
    * Properties not named by any `Preference` are left unaltered.
    */

  def update[T <: Object]( toUpdate: T, pref: Preference ): T =  
  {
    var workingCopy = toUpdate

    pref match {
      case grp: PreferenceGroup =>
        for (i <- 0 to grp.getPreferenceCount - 1)
          workingCopy = this.update( workingCopy, grp.getPreference(i) )
      case _ =>
        val binder = getBinder(pref).getOrElse(throw new NoBinderFor(pref))
        workingCopy = binder.update( pref, workingCopy ).asInstanceOf[T]
    }

    return workingCopy
  }

  /** Update an Android `View` (or `ViewGroup`) based on
    * the properties of the object `toShow`.
    * 
    * A `ViewGroup` is handled by iterating over its members.  (If
    * those contain nested `ViewGroup`s, we iterate over their members
    * too.)  Otherwise, we proceed as follows:
    *
    * If a binder has been declared for the particular `View` type
    * and the class of `toShow` (q.v. `bind`), then it is used to handle
    * the data transfer.  Otherwise, if the `View` has been bound
    * to a particular property type with `bindProperties`, and if `toShow`
    * is a [[org.postronicnet.util.ReflectiveProperties]] object, we 
    * attempt to get the name corresponding to the view's ID
    * (viz. [[org.positronicnet.ui.ResourceId]]), and look for an
    * appropriate property of that name.
    *
    * If we can't find any relevant declared UI Binding, and the supplied
    * `view` is a `TextView`, we effectively do `view.setText(toShow.toString)`.
    * A `TextView` that is the child of a supplied `ViewGroup` does not get
    * this treatment, to leave labels and the like in a complex layout of
    * some kind undisturbed.  Note that unlike for preferences, a child
    * `View` with no binder declared will just be ignored.
    */

  def show( toShow: Object, view: View ): Unit = 
    showInner( toShow, view, true )

  private
  def showInner( toShow: Object, view: View, topLvl: Boolean ): Unit = {
    view match {
      case grp: ViewGroup =>
        for (i <- 0 to grp.getChildCount - 1)
          showInner( toShow, grp.getChildAt( i ), false)
      case _ =>
        getBinder(view) match {
          case Some(binder) => binder.show( view, toShow )
          case None => 
            if (topLvl)
              view match {
                case txt:TextView => txt.setText( toShow.toString )
                case _ =>
              }
        }
    }
  }

  /** Use an Android `View` (or `ViewGroup`) to produce
    * an updated version of the [[org.positronicnet.util.ReflectiveProperties]]
    * object `toUpdate`.
    * 
    * A `ViewGroup` is handled by iterating over its members.  (If
    * those contain nested `ViewGroup`s, we iterate over their members
    * too.)  Otherwise, we proceed as follows:
    *
    * If a binder has been declared for the particular `View` type
    * and the class of `toShow` (q.v. `bind`), then it is used to handle
    * the data transfer.  Otherwise, if the `View` has been bound
    * to a particular property type with `bindProperties`, and if `toShow`
    * is a [[org.postronicnet.util.ReflectiveProperties]] object, we look
    * for a property of that type named by the `View`'s key
    * (viz. `getKey`).
    *
    * Properties not named by any `View` are left unaltered.
    */

  def update[T <: Object]( toUpdate: T, view: View ): T = 
  {
    var workingCopy = toUpdate

    view match {
      case grp: ViewGroup =>
        for (i <- 0 to grp.getChildCount - 1)
          workingCopy = this.update( workingCopy, grp.getChildAt( i ) )
      case _ =>
        getBinder(view) map { binder => 
          workingCopy = binder.update( view, workingCopy ).asInstanceOf[T] }
    }

    return workingCopy
  }
}

/** A utility [[org.positronicnet.ui.UiBinder]] object,
  * implementing default policies for applications that don't
  * need to extend them.
  */

object UiBinder extends UiBinder

/** Exception indicating that there is no binder for a particular widget
  */

class NoBinderFor( obj: Object )
  extends RuntimeException( "No UI binder declared for " + obj.toString )
