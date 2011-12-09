package org.positronicnet.ui.testUiBinder

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.ui.UiBinder
import org.positronicnet.ui.PropertyBinder
import org.positronicnet.ui.ResourceId
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

class CanaryBinder extends UiBinder
{
  bind[CanaryPref, Canary]( (_.showCanary(_)), (_.updateCanary(_)) )
}

// The spec itself

class UiBinderSpec
  extends Spec 
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
{
  val myContext = Robolectric.application
  val myBinder = new CanaryBinder

  // Some test fixtures.  We use subclasses of FooPreference defined below
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

  // Specs proper.

  describe( "bindings to properties" ) {
    it( "should be able to extract values" ) {
      myBinder.show( Canary( true, "yellow" ), prefs )
      flagCbox.isChecked should equal (true)
      blurbEtxt.getText  should equal ("yellow")
    }
    it( "should be able to set values" ) {
      flagCbox.setChecked( true )
      blurbEtxt.setText( "yellow" )
      val newCanary = myBinder.update( Canary( false, null ), prefs )
      newCanary.flag should equal (true)
      newCanary.blurb should equal ("yellow")
    }
  }

  describe( "bindings at class level" ) {
    it( "should be able to show" ) {
      val myPref = new CanaryPref( myContext )
      myBinder.show( Canary( true, "yellow" ), myPref )
      myPref.getText should equal ("yellow [flagged]")
    }
    it( "should be able to update" ) {
      val myPref = new CanaryPref( myContext )
      myPref.setText("blue")
      val updated = myBinder.update( Canary( true, "yellow" ), myPref ) 
      updated should equal (Canary (true, "blue"))
    }
  }

  describe( "ResourceId" ) {
    it( "should extract values from our dummy R class" ) {

      // We're in org.positronicnet.ui.testUiBinder, which has a
      // dummy R class declared for it in .../src/test/java.

      ResourceId.harvestAssociatedResources (this)
      ResourceId.toName (R.id.e)  should equal (Some("e"))
      ResourceId.toName (R.id.pi) should equal (Some("pi"))
      ResourceId.toName (53)      should equal (None)
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

class CanaryPref( ctx: Context )
  extends BogoEditTextPref( ctx )
{
  def showCanary( tweety: Canary ) =
    setText( 
      if (tweety.flag) 
        tweety.blurb + " [flagged]" 
      else
        tweety.blurb)

  def updateCanary( tweety: Canary ) =
    tweety.copy( blurb = getText )
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
