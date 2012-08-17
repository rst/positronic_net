package org.positronicnet.ui.testUiBinder

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.ui.{UiBinder, 
                             UiBindingsForSelfAndChildren,
                             ResourceId,
                             DoubleBindingException,
                             NoBinderFor,
                             IndexedSeqAdapter}
import org.positronicnet.util.ReflectiveProperties

import com.xtremelabs.robolectric.Robolectric
import org.positronicnet.test.RobolectricTests

import android.content.Context
import android.util.AttributeSet
import com.xtremelabs.robolectric.tester.android.util.TestAttributeSet

import android.preference.{Preference,PreferenceGroup,
                           CheckBoxPreference,EditTextPreference}

import android.view.{View,ViewGroup}
import android.widget.{TextView, EditText, CheckBox, Spinner,
                       LinearLayout, CheckedTextView}

// Entity being nominally bound to UI components (for now, prefs).
// (The 'flag' and 'blurb' are updated by our mutant CheckedTextView;
// the 'wrapUpdate' flag is there solely to indicate that the 'update'
// method of our CanaryBindingLinearLayout has been called.)

case class Canary( flag: Boolean, blurb: String, wrapUpdate: Boolean = false )
  extends ReflectiveProperties

// Entity not bound to any UI component (to verify safety properties)

case class Mockingbird( call: String )
  extends ReflectiveProperties

// Our binder

class CanaryBinder extends UiBinder
{
  bind[ CanaryPref, Canary ]( (_.showCanary(_)), (_.updateCanary(_)) )

  bind[ CheckedTextView, Canary ](
    ((view, canary) => {
      view.setText( canary.blurb )
      view.setChecked( canary.flag )
    }),
    ((view, canary) => {
      canary.copy( blurb = view.getText.toString,
                   flag  = view.isChecked )
    }))

  bind[ CanaryBindingLinearLayout, Canary ](
    ((view, canary) => view.bindCanary( canary )),
    ((view, canary) => view.updateCanary( canary ))
  )

  bind[ CanarySpinner, Canary ](
    (_.setCanary(_)), ((x,y) => x.getCanary))
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
  var randomTxtView: TextView = null
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

    randomTxtView = new TextView( myContext )
    randomTxtView.setText( "random" )

    views = new LinearLayout( myContext )
    views.addView( flagCboxView )
    views.addView( blurbEtxtView )
    views.addView( randomTxtView )
  }

  // Specs proper.

  describe( "declarations of bindings" ) {

    // Mostly implicitly tested through tests of the declared bindings
    // below, but there is this...

    it ( "should whine about double declarations" ) {
      intercept [DoubleBindingException] {
        myBinder.bind[CanaryPref, Canary]( 
          (_.showCanary(_)), (_.updateCanary(_)) )
      }
    }
  }

  describe( "bindings to preferences" ) {
    describe( "of properties" ) {
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
    describe( "at class level" ) {
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
    describe ("should whine about missing bindings") {
      it( "on show" ) {
        intercept[ NoBinderFor ]{
          myBinder.show( List(3,3), prefs )
        }
      }
      it( "on update" ) {
        intercept[ NoBinderFor ]{
          myBinder.update( List(3,3), prefs )
        }
      }
    }
  }

  describe( "bindings of views" ) {
    describe( "to properties" ) {
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

    describe( "at class level" ) {
      it( "should be able to show" ) {
        val view = new HackedCheckedTextView( myContext )
        myBinder.show( Canary( true, "yellow" ), view )
        view.getText should equal ("yellow")
        view.isChecked should equal (true)
        randomTxtView.getText should equal ("random") // unchanged
      }
      it( "should be able to update" ) {
        val view = new HackedCheckedTextView( myContext )
        view.setText("blue")
        val updated = myBinder.update( Canary( true, "yellow" ), view ) 
        updated should equal (Canary (false, "blue"))
      }
    }

    describe( "of compound views (and to objects)" ) {
      it ("should find explicit bindings to ViewGroups") {
        val view = new CanarySpinner( myContext )
        myBinder.show( Canary( true, "yellow" ), view )
        view.getCanary should equal ( Canary( true, "yellow" ))
      }
      it ("should update from explicit bindings to ViewGroups") {
        val view = new CanarySpinner( myContext )
        val myCanary = Canary( false, "green" )
        view.setCanary( myCanary )
        myBinder.update( Canary( true, "yellow" ), view) should equal (myCanary)
      }
      it ("shouldn't blow up on displaying objects of a different type") {
        val view = new CanarySpinner( myContext )
        myBinder.show( Mockingbird( "heckle" ), view )
      }
    }

    describe( "of groups which allow bindings to children") {

      def makeCanaryBindingGroup = {
        val grp = new CanaryBindingLinearLayout( myContext )
        val btn = new HackedCheckedTextView( myContext )
        grp.addView( btn )
        (grp, btn)
      }

      it ("should show in both group and child") {
        val myCanary = Canary( true, "yellow" )
        makeCanaryBindingGroup match {
          case (grp, btn) =>
            myBinder.show( myCanary, grp )
            grp.canary should be (myCanary)
            btn.getText.toString should be (myCanary.blurb)
        }
      }

      it ("should show update from both group and child") {
        makeCanaryBindingGroup match {
          case (grp, btn) =>
            btn.setText("blue")
            val upCanary = myBinder.update( Canary( false, "yellow"), grp )
            upCanary.blurb should be ("blue")
            upCanary.wrapUpdate should be (true)
        }
      }

    }
  }

  describe( "binding to a random TextView" ) {
    it ("should show the result of toString") {
      val canary = Canary ( true, "yellow" )
      myBinder.show( canary, randomTxtView )
      randomTxtView.getText.toString should equal (canary.toString)
    }
    it ("should leave an 'update' unaltered") {
      val canary = Canary ( true, "yellow" )
      val updated = myBinder.update( canary, randomTxtView )
      updated should equal (canary)
    }
  }

  describe( "ResourceId" ) {
    it( "should extract values from our dummy R class" ) {
      ResourceId.toName (R.id.e)  should equal (Some("e"))
      ResourceId.toName (R.id.pi) should equal (Some("pi"))
      ResourceId.toName (53)      should equal (None)
    }
  }

  describe( "full framework integration" ) {
    it( "should harvest resource IDs when an Activity is created" ) {
      ResourceId.clear
      ResourceId.toName (R.id.e) should equal (None)
      val dummy = new ActivityInThisPackage
      ResourceId.toName (R.id.e) should equal (Some("e"))
    }
    describe( "integration with IndexedSeqAdapter" ) {
      it( "should properly support default behavior" ) {
        val adapter = new IndexedSeqAdapter( IndexedSeq(Canary(true, "tweety")))
        val sillyView = new TextView( myContext )
        val myCanary = Canary(false, "heckle")
        adapter.bindView( sillyView, myCanary )
        sillyView.getText.toString should equal (myCanary.toString)
      }
      it( "should allow override binders" ) {
        val adapter = new IndexedSeqAdapter( IndexedSeq(Canary(true, "tweety")),
                                             binder = myBinder )
        adapter.bindView( views, Canary(true, "jekyll" ))
        flagCboxView.isChecked         should equal (true)
        blurbEtxtView.getText.toString should equal ("jekyll")
      }
    }
  }

  // Testing the test infrastructure... sigh.

  describe( "mocks (sigh...)" ) {
    it( "should have proper keys, ids, etc." ) {

      flagCbox.getKey should equal( "flag" )
      blurbEtxt.getKey should equal( "blurb" )
      prefs.getPreferenceCount should equal (2)

      flagCboxView.getId should equal (R.id.flag)
      blurbEtxtView.getId should equal (R.id.blurb)
      views.getChildCount should equal (3) // including random text view.
    }
  }
}

// Nonce classes for further test support.

class ActivityInThisPackage 
  extends org.positronicnet.ui.PositronicActivity

// Robolectric shadows for Preference are... incomplete.  To avoid requiring
// a private Robolectric build, we do this. 
//
// And as a bonus... we now also test that we find bindings for subclasses!
// Bletch.

class BogoCheckBoxPref( ctx: Context )
  extends CheckBoxPreference( ctx )
{
  var checked: Boolean = false

  override def isChecked = checked
  override def setChecked( b: Boolean ): Unit = { checked = b }
}

class BogoEditTextPref( ctx: Context )
  extends EditTextPreference( ctx )
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

// Similar hackery for incomplete CheckedTextView emulation.

class HackedCheckedTextView( ctx: Context )
  extends CheckedTextView( ctx )
{
  var checked: Boolean = false
  override def isChecked = checked
  override def setChecked( b: Boolean ):Unit = { checked = b }
}

class CanarySpinner( ctx: Context ) extends Spinner( ctx )
{
  var canary: Canary = null

  def getCanary = canary
  def setCanary( c: Canary ) = { this.canary = c }
}

// And a container that binds to Canary objects, while also allowing
// bindings to its children...

class CanaryBindingLinearLayout( ctx: Context )
  extends LinearLayout( ctx )
  with UiBindingsForSelfAndChildren
{
  var canary: Canary = null
  def bindCanary( canary: Canary ) = this.canary = canary
  def updateCanary( canary: Canary ) = canary.copy( wrapUpdate = true )
}
