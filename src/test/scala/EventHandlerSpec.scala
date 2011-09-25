package org.positronicnet.test

import org.scalatest._

import android.app.Activity
import android.view.KeyEvent
import org.positronicnet.ui.PositronicTextView

class EventHandlerSpec
  extends Spec with RobolectricTests
{
  class DummyActivity extends Activity

  lazy val dummyActivity = new DummyActivity

  describe( "click handling" ){
    it ("should call the handler") {

      var callHappened: Boolean = false
      val tv = new PositronicTextView( dummyActivity )

      tv.onClick{ callHappened = true }
      assert( !callHappened )

      tv.performClick
      assert( callHappened )
    }
  }

  describe( "key handling" ) {

    // A view, and some handlers for it.
    //
    // It's a "lazy" attribute, so it only actually gets built in the
    // Robolectric shadow-test-suite.

    var hitMetaEnter = false
    var hitEnter     = false
    var randomCode   = -1
    
    lazy val cookedView = {
      val v = new PositronicTextView( dummyActivity )

      v.onKey( KeyEvent.KEYCODE_ENTER ){ hitEnter = true }
      v.onKey( KeyEvent.KEYCODE_ENTER, KeyEvent.META_ALT_ON ) {
        hitMetaEnter = true
      }
      v.onKey{ (kc, ev) => {randomCode = kc; true} }

      v
    }

    // Faking up keystrokes.  We reset the state variables tweaked by
    // our handlers first.  (And we have to tweak the listener directly
    // to work because Robolectric-1.0RC1 doesn't emulate dispatchKeyEvent,
    // or the rest of the KeyEvent machinery... and I'd really like people
    // to be able to run the tests without a custom Robolectric build.)

    def hitKey( code: Int, metaState: Int = 0, 
                action: Int = KeyEvent.ACTION_DOWN ) = 
    {
      hitMetaEnter = false
      hitEnter     = false
      randomCode   = -1

      cookedView.positronicKeyDispatch( code, metaState, action, 
                                        new KeyEvent( action, code ))
    }

    // Seeing what happened

    def assertState( state: String, 
                     shouldHitEnter: Boolean, shouldHitMEnter: Boolean, 
                     kc: Int = -1 ) = {
      assert( hitEnter     == shouldHitEnter,  state + " hitEnter" )
      assert( hitMetaEnter == shouldHitMEnter, state + " hitMetaEnter" )
      assert( randomCode   == kc,              state + " keyCode" )
    }

    // Test cases

    it( "should process declared unmodified key correctly" ) {
      hitKey( KeyEvent.KEYCODE_ENTER )
      assertState( "plain enter", true, false )
    }
    it( "should process declared meta-modified key correctly" ) {
      hitKey( KeyEvent.KEYCODE_ENTER, metaState = KeyEvent.META_ALT_ON )
      assertState( "meta-enter", false, true )
    }
    it( "should process random keys correctly" ) {
      hitKey( 'A' )
      assertState( "random", false, false, 'A' )
    }
    it( "should process declared per-code handlers only on ACTION_DOWN" ) {
      hitKey( KeyEvent.KEYCODE_ENTER, action = KeyEvent.ACTION_UP )
      assertState( "keydown enter", false, false, KeyEvent.KEYCODE_ENTER )
    }
  }
}
