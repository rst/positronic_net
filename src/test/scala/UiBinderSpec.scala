package org.positronicnet.ui.testUiBinder

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.ui.UiBinder
import org.positronicnet.ui.PropertyBinder
import org.positronicnet.util.ReflectiveProperties

import com.xtremelabs.robolectric.Robolectric
import org.positronicnet.test.RobolectricTests

import android.preference.{Preference,CheckBoxPreference,EditTextPreference}
import android.content.Context

// Entity being nominally bound to UI components (for now, prefs).

case class Canary( flag: Boolean, blurb: String )
  extends ReflectiveProperties

class UiBinderSpec
  extends Spec 
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
{
  val myContext = Robolectric.application

  // Our test prefs.  We use subclasses of FooPreference defined below
  // because Robolectrics mocking of some relevant features is... incomplete.

  var flagCbox: CheckBoxPreference = null
  var blurbEtxt: EditTextPreference = null

  override def beforeEach = {
    flagCbox = new BogoCheckBoxPref( myContext )
    flagCbox.setKey( "flag" )

    blurbEtxt = new BogoEditTextPref( myContext )
    blurbEtxt.setKey( "blurb" )
  }

  describe( "PropertyBinder" ) {
    it( "should be able to extract values" ) {
      UiBinder.show( Canary( true, "yellow" ), List( flagCbox, blurbEtxt ) )
      flagCbox.isChecked should equal (true)
      blurbEtxt.getText  should equal ("yellow")
    }
    it( "should be able to set values" ) {
      flagCbox.setChecked( true )
      blurbEtxt.setText( "yellow" )
      val newCanary = UiBinder.update[ Canary ]( Canary( false, null ), 
                                                 List( flagCbox, blurbEtxt ))
      System.err.println( "new canary is: " + newCanary.toString )
      newCanary.flag should equal (true)
      newCanary.blurb should equal ("yellow")
    }
  }

  // Sanity checks on invisible infrastructure.
  //
  // We're in a subpackage of org.positronicnet.ui, or this stuff
  // (declared private[ui]) wouldn't even be visible.

  describe( "BindingManager" ) {
    it( "should find declared bindings" ) {
      PropertyBinder.findBinder( flagCbox ) should not equal (None)
      PropertyBinder.findBinder( blurbEtxt ) should not equal (None)
    }
  }

  // Testing the test infrastructure... sigh.

  describe( "preference mocks" ) {
    it( "should have proper keys" ) {
      flagCbox.getKey should equal( "flag" )
      blurbEtxt.getKey should equal( "blurb" )
    }
  }
}

// Robolectric shadows for Preference are... awkward.  To avoid requiring
// a private Robolectric build, we do this. 
//
// And as a bonus... we now also test that we find bindings for subclasses!
// Bletch.

trait RobolectricPrefBugWorkaround extends Preference {

  var key: String = null

  override def getKey = key
  override def setKey( s: String ): Unit = { key = s }
}

class BogoCheckBoxPref( ctx: Context )
  extends CheckBoxPreference( ctx ) with RobolectricPrefBugWorkaround
{
  var checked: Boolean = false

  override def isChecked = checked
  override def setChecked( b: Boolean ): Unit = { checked = b }
}

class BogoEditTextPref( ctx: Context )
  extends EditTextPreference( ctx ) with RobolectricPrefBugWorkaround
{
  var txt: String = null

  override def getText = txt
  override def setText( s: String ): Unit = { txt = s }
}

