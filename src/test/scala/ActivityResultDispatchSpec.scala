package org.positronicnet.ui.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.ui._

import org.positronicnet.test.RobolectricTests
import com.xtremelabs.robolectric.Robolectric
import com.xtremelabs.robolectric.tester.android.util.TestAttributeSet

import android.content.{Context, Intent}
import android.util.AttributeSet
import android.net.Uri

class DummyActivity 
  extends PositronicActivity 
  with ActivityResultDispatch
{
  var lastStartIntent: Intent = null

  override def startActivity( intent: Intent ) =
    lastStartIntent = intent
}

class DispatchingTextView( ctx: Context, attrs: AttributeSet )
  extends PositronicTextView( ctx, attrs )
  with ActivityResultDispatchClient
{
  // Key to identify the "equivalent" ActivityResultDispatchClient
  // in a process that was recreated after a system kill.

  def activityResultDispatchKey = "fred"

  // Make protected trait methods public, to simplify test code...

  override def registerForActivityResultDispatch =
    super.registerForActivityResultDispatch

  override def awaitActivityResult( intent: Intent, 
                                    methodName: String,
                                    extraArgs: Any*
                                  ) = 
    super.awaitActivityResult( intent, methodName, extraArgs: _* )

  override def startActivity( intent: Intent ) =
    super.startActivity( intent )

  // response handler...

  var respCode: Int = -1111111
  var data: Intent = null
  var arg1: String = null
  var arg2: Integer = null

  def aMethod( code: Int, data: Intent, arg1: String, arg2: Int )={
    this.respCode = code
    this.data = data
    this.arg1 = arg1
    this.arg2 = arg2
  }
}

class ActivityResultDispatchSpec
  extends Spec
  with ShouldMatchers
  with RobolectricTests
{
  describe( "activity result dispatch" ) {
    it ("should work through lifecycle stages") {

      // Each step here is necessary to set up the next, so it's hard
      // to write fully independent tests without a lot of overhead.
      // Hence, this...

      // Step 0:  Create objects

      val activity = new DummyActivity
      val dispatchClient = new DispatchingTextView( activity, 
                                                    new TestAttributeSet )

      // Step 1:  Register for dispatch services

      val dispatchKey = dispatchClient.activityResultDispatchKey

      dispatchClient.registerForActivityResultDispatch
      activity.activityResponders should have size (1)
      activity.activityResponders( dispatchKey ) should be (dispatchClient)

      // Step 2:  Try to trigger and register...

      val reqCode = activity.nextActivityRequestCode
      val intent = new Intent("org.positronicnet.dummy")

      dispatchClient.awaitActivityResult( intent, "aMethod", "a string", 42 )

      activity.nextActivityRequestCode should be (reqCode + 1)
      activity.pendingResponses should have size (1)
      activity.pendingResponses( reqCode ) should be (
        PendingResponse( dispatchKey, "aMethod", Seq( "a string", 42 )))

      // Step 3: Try to dispatch a result (to a dummy method which will set
      // a bunch of accessible fields to the values of its arguments)

      val respCode = 42
      val uri = Uri.parse("content://org.positronicnet.dummy")
      val respIntent = new Intent("org.positronicnet.dummyR", uri)
      activity.onActivityResult( reqCode, respCode, respIntent )

      dispatchClient.respCode should equal (respCode)
      dispatchClient.data.getData should equal (uri)
      dispatchClient.arg1 should be ("a string")
      dispatchClient.arg2 should be (42)
    } 
  }

  describe( "activity dispatch without result" ) {
    it( "should find and go through the dispatcher" ) {

      val activity = new DummyActivity
      val dispatchClient = new DispatchingTextView( activity, 
                                                    new TestAttributeSet )
      val intent = new Intent("org.positronicnet.dummy")

      dispatchClient.startActivity( intent )
      activity.lastStartIntent should be (intent)
    }
  }
}
