package org.positronicnet.ui

import android.preference.{Preference,PreferenceGroup,
                           CheckBoxPreference,EditTextPreference}

import org.positronicnet.util.ReflectiveProperties
import org.positronicnet.util.LensFactory

// Feasibility study for binding UI components.
// May ultimately want to sugar the syntax here.
//
// (May also want to ultimately factor out commonalities with 
// code that walks a View hierarchy.)

private [ui]
abstract class UiBinding {
  def show (widget: Object, props: ReflectiveProperties): Unit
  def update (widget: Object, props: ReflectiveProperties): ReflectiveProperties
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
        throw new RuntimeException( "Not introspecting on resource IDs yet" )
    }

  val lensFactory = LensFactory.forPropertyType[ TProp ]
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

  def show (widget: Object, props: ReflectiveProperties) = {
    val wwidget = widget.asInstanceOf[ TWidget ]
    writeFunc( wwidget, lens( wwidget, props ).getter( props ) )
  }

  def update (widget: Object, props: ReflectiveProperties) = {
    val wwidget = widget.asInstanceOf[ TWidget ]
    val result = lens( wwidget, props ).setter( props, readFunc( wwidget ))
    result.asInstanceOf[ ReflectiveProperties ]
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

class PropertyBinder extends BindingManager {

  /** Declare that widgets of type `TWidget` can be used to render
    * or set properties of type `TProp`.  The caller must supply two
    * functions to manage the mechanics of the shuffling:
    * a `readFunc` to get a `TProp` out of a `TWidget`,
    * and a `writeFunc` to put a `TProp` into a `TWidget`.  Generally
    * invoked from within a constructor.
    * 
    * Sample usage, representing the default behavior:
    * {{{
    *     bindProperties[ EditTextPreference, String ](
    *       (_.getText), (_.setText( _ )))
    *
    *     bindProperties[ CheckBoxPreference, Boolean ](
    *       (_.isChecked), (_.setChecked( _ )))
    * }}}
    */

  def bindProperties[ TWidget : ClassManifest, TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  ) =
    noteBinding( classManifest[ TWidget ].erasure,
                 new PropertyBinding( readFunc, writeFunc ) )

  // A few stock bindings --- checkbox prefs can set boolean properties;
  // EditTextPreferences can set String properties.  Subclasses can add more.

  bindProperties[ EditTextPreference, String ](
    (_.getText), (_.setText( _ )))

  bindProperties[ CheckBoxPreference, Boolean ](
    (_.isChecked), (_.setChecked( _ )))
}

/** A utility [[org.positronicnet.ui.PropertyBinder]] object,
  * implementing default policies for applications that don't
  * need to extend them.
  */

object PropertyBinder extends PropertyBinder

class UiBinder( val propBinder: PropertyBinder )
  extends BindingManager 
{
  private
  def getBinder( obj: Object ) = {
    findBinder( obj ) match {
      case Some (binder) => binder
      case None => PropertyBinder.findBinder( obj ) match {
        case Some (binder) => binder
        case None => 
          throw new RuntimeException(
            "No UI binder declared for class " + obj.getClass.toString)
      }
    }
  }  

  def show( props: ReflectiveProperties, prefs: Iterable[Preference] ) = {
    for (pref <- prefs) 
      getBinder( pref ).show( pref, props )
  }
  
  def update[T <: ReflectiveProperties]( props: T,
                                         prefs: Iterable[Preference] ): T = {
    var workingCopy: ReflectiveProperties = props
    for (pref <- prefs)
      workingCopy = getBinder( pref ).update( pref, workingCopy )

    return workingCopy.asInstanceOf[T]
  }
}

object UiBinder extends UiBinder( PropertyBinder )
