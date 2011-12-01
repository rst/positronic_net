package org.positronicnet.ui

import android.preference.{Preference,PreferenceScreen,
                           CheckBoxPreference,EditTextPreference}

import org.positronicnet.util.ReflectiveProperties
import org.positronicnet.util.LensFactory

// Feasibility study for binding UI components.
// May ultimately want to sugar the syntax here.
//
// (May also want to ultimately factor out commonalities with 
// code that walks a View hierarchy.)

object UiBinderImplicits {

  def getPrefs( screen: PreferenceScreen ): Iterable[Preference] = {

    // The following looks hinky, but I can't find another documented
    // way to get a complete list of the actual prefs!

    val adapter = screen.getRootAdapter

    for (i <- 0 to adapter.getCount) 
    yield adapter.getItem( i ).asInstanceOf[ Preference ]
  }

  // Dummy method to force the constructor on this object to get called...

  private [ui]
  def setup: Unit = null

  // A few stock bindings --- checkbox prefs can set boolean properties;
  // EditTextPreferences can set String properties.

  PropertyBinder.declare[ CheckBoxPreference, Boolean ](
    (_.isChecked), (_.setChecked( _ )))

  PropertyBinder.declare[ EditTextPreference, String ](
    (_.getText), (_.setText( _ ) ))
}

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

object PropertyBinder extends BindingManager {

  // Make sure that the stock binding declarations in UiBinderImplicits
  // get called before we're asked to do anything else!

  UiBinderImplicits.setup

  def declare[ TWidget : ClassManifest, TProp : ClassManifest ](
    readFunc: TWidget => TProp,
    writeFunc: (TWidget, TProp) => Unit
  ) =
    noteBinding( classManifest[ TWidget ].erasure,
                 new PropertyBinding( readFunc, writeFunc ) )
}

object UiBinder extends BindingManager {

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
  
  def update[T <: ReflectiveProperties]( props: ReflectiveProperties,
                                         prefs: Iterable[Preference] ): T = {
    var workingCopy = props
    for (pref <- prefs)
      workingCopy = getBinder( pref ).update( pref, workingCopy )

    return workingCopy.asInstanceOf[T]
  }
}


