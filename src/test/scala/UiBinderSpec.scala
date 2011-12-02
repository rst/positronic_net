package org.positronicnet.ui.testUiBinder

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.ui.UiBinder
import org.positronicnet.ui.PropertyBinder
import org.positronicnet.util.ReflectiveProperties

import com.xtremelabs.robolectric.Robolectric
import org.positronicnet.test.RobolectricTests

import android.content.Context
import android.util.AttributeSet
import com.xtremelabs.robolectric.tester.android.util.TestAttributeSet

import android.preference.{Preference,PreferenceGroup,
                           CheckBoxPreference,EditTextPreference}

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

  var flagCbox:  CheckBoxPreference = null
  var blurbEtxt: EditTextPreference = null
  var prefs:     PreferenceGroup = null

  override def beforeEach = {
    flagCbox = new BogoCheckBoxPref( myContext )
    flagCbox.setKey( "flag" )

    blurbEtxt = new BogoEditTextPref( myContext )
    blurbEtxt.setKey( "blurb" )

    prefs = new BogoPreferenceGroup( myContext, new TestAttributeSet )
    prefs.addPreference( flagCbox )
    prefs.addPreference( blurbEtxt )
  }

  describe( "PropertyBinder" ) {
    it( "should be able to extract values" ) {
      UiBinder.show( Canary( true, "yellow" ), prefs )
      flagCbox.isChecked should equal (true)
      blurbEtxt.getText  should equal ("yellow")
    }
    it( "should be able to set values" ) {
      flagCbox.setChecked( true )
      blurbEtxt.setText( "yellow" )
      val newCanary = UiBinder.update( Canary( false, null ), prefs )
      newCanary.flag should equal (true)
      newCanary.blurb should equal ("yellow")
    }
  }

  // Testing the test infrastructure... sigh.

  describe( "preference mocks" ) {
    it( "should have proper keys" ) {
      flagCbox.getKey should equal( "flag" )
      blurbEtxt.getKey should equal( "blurb" )
      prefs.getPreferenceCount should equal (2)
    }
  }
}

// Robolectric shadows for Preference are... incomplete.  To avoid requiring
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

class BogoPreferenceGroup( ctx: Context, attrs: AttributeSet ) 
  extends PreferenceGroup( ctx, attrs )
{
  var prefs: Seq[Preference] = Seq.empty
  override def addPreference( pref: Preference ): Boolean = { 
    prefs = prefs :+ pref
    return true
  }
  override def getPreferenceCount = prefs.size
  override def getPreference( i: Int ) = prefs( i )
}
