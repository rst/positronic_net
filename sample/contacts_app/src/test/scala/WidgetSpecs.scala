package org.positronicnet.sample.contacts.test

import org.positronicnet.sample.contacts._
import org.positronicnet.ui._
import org.positronicnet.util.ReflectUtils

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.test.RobolectricTests
import com.xtremelabs.robolectric.Robolectric
import com.xtremelabs.robolectric.tester.android.util.TestAttributeSet

import android.provider.ContactsContract.CommonDataKinds

class WidgetSpec
  extends Spec
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
{
  // Can't access CommonDataKinds.Phone.TYPE_CUSTOM from Scala as
  // such, since it's actually declared on 'BaseTypes', and Scala
  // unsurprisingly can't track down inherited static fields.  The
  // following works, at least for now...

  val TYPE_HOME   = CommonDataKinds.Phone.TYPE_HOME
  val TYPE_CUSTOM = CommonDataKinds.BaseTypes.TYPE_CUSTOM

  val homePhone = (new Phone).setProperty( "recType", TYPE_HOME )
  val customPhone = 
    (new Phone).setProperty( "recType", TYPE_CUSTOM )
               .setProperty( "label", "FOAF Mobile" )

  ResourceId.harvestAssociatedResources (this)

  override def beforeEach =
    Res.openInContext( Robolectric.application ) // for "Res.ources".  Sigh...

  // equality comparisons on TypeFields don't work, so...

  def assertTypeField( tf: TypeField, recType: Int, label: String ) = {
    tf.recType should equal (recType)
    tf.label   should equal (label)
  }

  describe( "type field handling" ) {
    it( "should render standard labels correctly" ) {
      // Can't test all in Robolectric.  Sigh...
      assertTypeField( homePhone.recordType, TYPE_HOME, null )
    }
    it( "should render custom labels correctly" ) {
      assertTypeField( customPhone.recordType, TYPE_CUSTOM, "FOAF Mobile" )
    }
    it( "should show up as a reflective property" ) {
      assertTypeField( homePhone.getProperty[ TypeField ]("recordType"), 
                       TYPE_HOME, null )
      assertTypeField( customPhone.getProperty[ TypeField ]("recordType"), 
                       TYPE_CUSTOM, "FOAF Mobile" )
    }
  }

}
