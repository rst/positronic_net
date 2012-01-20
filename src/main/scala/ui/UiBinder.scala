package org.positronicnet.ui

import android.preference.{Preference,PreferenceGroup,
                           CheckBoxPreference,EditTextPreference}

import android.view.{View,ViewGroup}
import android.widget.{TextView,CheckBox}
import android.util.Log

import org.positronicnet.util.{ReflectiveProperties,
                               PropertyLens,
                               PropertyLensFactory}

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
class BindingManager[TBinding] {

  protected
  var bindingMap: Map[Class[_], TBinding] = Map.empty

  protected
  def noteBinding( klass: Class[_], binder: TBinding, kind: String ) =
    if (bindingMap.isDefinedAt( klass ))
      throw new DoubleBindingException( klass, kind )
    else
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
  def findBinderForSuperclass( klass: Class[_] ): Option[TBinding] = {
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
class PropertyBinding[ TWidget <: Object, TData <: Object, TProp ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit,
    lens: PropertyLens[ TData, TProp ])
  extends UiBinding
{
  def show (widget: Object, props: Object) = {
    val wwidget = widget.asInstanceOf[ TWidget ]
    val pprops = props.asInstanceOf[ TData ]
    writeFunc( wwidget, lens.getter( pprops ) )
  }

  def update (widget: Object, props: Object) = {
    val wwidget = widget.asInstanceOf[ TWidget ]
    val pprops = props.asInstanceOf[ TData ]
    val result = lens.setter( pprops, readFunc( wwidget ))
    result.asInstanceOf[ TData ]
  }
}

private [ui]
class PropertyBindingFactory[ TWidget <: Object, 
                              TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  )
{
  val lensFactory = PropertyLensFactory.forPropertyType[ TProp ]
  val propKlass = classManifest[ TProp ].erasure
  var bindingMap: Map [(Class[_], String), 
                       Option [PropertyBinding[_,_,_]]] = 
    Map.empty

  def findBinder[TData <: Object](propName: String,
                                  dataKlass: Class[TData])
      :Option[ PropertyBinding[ TWidget, TData, TProp ]] = 
  {
    val option = bindingMap.get( (dataKlass, propName) ) match {
      case Some(option) => 
        option
      case None => {
        Log.d( "UiBinder",
               "Seeking property of type " + propKlass + " named '" +
               propName + "' on " + dataKlass )
        val newOption = 
          lensFactory.forProperty( dataKlass, propName ) map { lens =>
            new PropertyBinding( readFunc, writeFunc, lens) }
        bindingMap += ((dataKlass, propName) -> newOption)
        newOption
      }
    }
    option.asInstanceOf[ Option [PropertyBinding[ TWidget, TData, TProp ]]]
  }
}

private [ui]
class PropertyBinderManager
  extends BindingManager[PropertyBindingFactory[_,_]] 
{
  // A factory factory.  Sigh...

  def propertyName( widget: Object ) =
    widget match {
      case pref: android.preference.Preference => pref.getKey
      case view: android.view.View =>
        ResourceId.toName( view.getId ).getOrElse("")
    }

  def bindProperties[ TWidget : ClassManifest, TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  ) =
    noteBinding( classManifest[ TWidget ].erasure,
                 new PropertyBindingFactory( readFunc, writeFunc ),
                 "property"
               )

  def findPropertyBinder( widget: Object, obj: Object ) =
    findBinder( widget ) match { 
      case None => 
        None
      case Some(factory) =>
        val klass = obj.getClass.asInstanceOf[ Class[T] forSome{type T<:Object}]
        factory.findBinder( propertyName( widget ), klass ) 
    }
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
  extends BindingManager[SimpleBinding[_,_]]
{
  private val propertyBinders = new PropertyBinderManager

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
    * or `android.view.View`.
    * 
    * The default behavior is specified as follows; declarations for
    * other types may be specified similarly, often in a subclass constructor:
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
    propertyBinders.bindProperties[ TWidget, TProp ](readFunc, writeFunc)

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
                 new SimpleBinding( showFunc, updateFunc ),
                 "class-level"
               )

  /** Get Some of a UiBinder that can be used to mediate between
    * 'widget' and 'object' (or some appropriately chosen property),
    * or none.
    */

  protected
  def getBinder( widget: Object, target: Object ):Option[UiBinding] =
    this.findBinder( widget ).orElse(
      propertyBinders.findPropertyBinder (widget, target))

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
        val binder = 
          getBinder(pref, toShow).getOrElse(throw new NoBinderFor(pref))

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

        val binder = 
          getBinder(pref, toUpdate).getOrElse(throw new NoBinderFor(pref))

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
    * XXX not working:
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

    getBinder(view, toShow) match {

      case Some(binder) => 
        binder.show( view, toShow )

      case None =>
        view match {
          case grp: ViewGroup =>
            for (i <- 0 to grp.getChildCount - 1)
              showInner( toShow, grp.getChildAt( i ), false)
          case txt:TextView =>
            if (topLvl)
              txt.setText( toShow.toString )
          case _ => // do nothing
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
        getBinder(view, toUpdate) map { binder => 
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

/** Exception indicating that there is no binder for a particular widget */

class NoBinderFor( obj: Object )
  extends RuntimeException( "No UI binder declared for " + obj.toString )

/** Exception indicating that a widget already has a binder of a particular
  * type
  */

class DoubleBindingException( klass: Class[_], kind: String )
  extends RuntimeException( klass.toString + " already has a " + 
                            kind + " binding.  " +
                            "(Consider subclassing if it needs more than one?)"
                          )
