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

import android.view.{View,ViewGroup}
import android.widget.{TextView, EditText, CheckBox,
                       LinearLayout, CheckedTextView}

// Entity being nominally bound to UI components (for now, prefs).

case class Canary( flag: Boolean, blurb: String )
  extends ReflectiveProperties

class CanaryBinder extends UiBinder
{
  bind[CanaryPref, Canary]( (_.showCanary(_)), (_.updateCanary(_)) )

  bind[CheckedTextView, Canary](
    ((view, canary) => {
      view.setText( canary.blurb )
      view.setChecked( canary.flag )
    }),
    ((view, canary) => {
      canary.copy( blurb = view.getText.toString,
                   flag  = view.isChecked )
    }))
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

  // We're in org.positronicnet.ui.testUiBinder, which has a
  // dummy R class declared for it in .../src/test/java.

  ResourceId.harvestAssociatedResources (this)

  // Some test fixtures.  We use subclasses of FooPreference defined below
  // because Robolectrics mocking of some relevant features is... incomplete.

  var flagCbox:  CheckBoxPreference = null
  var blurbEtxt: EditTextPreference = null
  var prefs:     PreferenceGroup = null

  var flagCboxView:  CheckBox = null
  var blurbEtxtView: EditText = null
  var views:         ViewGroup = null

  override def beforeEach = {

    flagCbox = new BogoCheckBoxPref( myContext )
    flagCbox.setKey( "flag" )

    blurbEtxt = new BogoEditTextPref( myContext )
    blurbEtxt.setKey( "blurb" )

    prefs = new BogoPreferenceGroup( myContext, new TestAttributeSet )
    prefs.addPreference( flagCbox )
    prefs.addPreference( blurbEtxt )

    flagCboxView = new CheckBox( myContext )
    flagCboxView.setId( R.id.flag )

    blurbEtxtView = new EditText( myContext )
    blurbEtxtView.setId( R.id.blurb )

    views = new LinearLayout( myContext )
    views.addView( flagCboxView )
    views.addView( blurbEtxtView )
  }

  // Specs proper.

  describe( "bindings of preferences to properties" ) {
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

  describe( "bindings to preferences at class level" ) {
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

  describe( "bindings of views to properties" ) {
    it( "should be able to extract values" ) {
      myBinder.show( Canary( true, "yellow" ), views )
      flagCboxView.isChecked         should equal (true)
      blurbEtxtView.getText.toString should equal ("yellow")
    }
    it( "should be able to set values" ) {
      flagCboxView.setChecked( true )
      blurbEtxtView.setText( "yellow" )
      val newCanary = myBinder.update( Canary( false, null ), views )
      newCanary.flag should equal (true)
      newCanary.blurb should equal ("yellow")
    }
  }

  describe( "bindings to views at class level" ) {
    it( "should be able to show" ) {
      val view = new HackedCheckedTextView( myContext )
      myBinder.show( Canary( true, "yellow" ), view )
      view.getText should equal ("yellow")
      view.isChecked should equal (true)
    }
    it( "should be able to update" ) {
      val view = new HackedCheckedTextView( myContext )
      view.setText("blue")
      val updated = myBinder.update( Canary( true, "yellow" ), view ) 
      updated should equal (Canary (false, "blue"))
    }
  }

  describe( "ResourceId" ) {
    it( "should extract values from our dummy R class" ) {
      ResourceId.toName (R.id.e)  should equal (Some("e"))
      ResourceId.toName (R.id.pi) should equal (Some("pi"))
      ResourceId.toName (53)      should equal (None)
    }
  }

  // Testing the test infrastructure... sigh.

  describe( "mocks (sigh...)" ) {
    it( "should have proper keys" ) {

      flagCbox.getKey should equal( "flag" )
      blurbEtxt.getKey should equal( "blurb" )
      prefs.getPreferenceCount should equal (2)

      flagCboxView.getId should equal (R.id.flag)
      blurbEtxtView.getId should equal (R.id.blurb)
      views.getChildCount should equal (2)
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

class HackedCheckedTextView( ctx: Context )
  extends CheckedTextView( ctx )
{
  var checked: Boolean = false
  override def isChecked = checked
  override def setChecked( b: Boolean ):Unit = { checked = b }
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
