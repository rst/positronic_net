package org.positronicnet.sample.contacts.test

import org.positronicnet.sample.contacts._

import org.positronicnet.content.PositronicContentResolver

import org.scalatest._
import org.positronicnet.test.RobolectricTests
import com.xtremelabs.robolectric.Robolectric
import org.scalatest.matchers.ShouldMatchers

class ModelSpec
  extends Spec
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
{
  override def beforeEach = 
    PositronicContentResolver.openInContext( Robolectric.application )

  override def afterEach = 
    PositronicContentResolver.close

  // Here, I'm letting the tests know that DISPLAY_NAME is an alias for DATA1,
  // and the other fields are DATAX for X <> 1.  (I'm keeping that knowledge
  // out of the app to try to keep it readable, but it's handy here...)

  describe( "structured name mapper" ) {

    it ("should send display name only if structured fields unset") {

      val name = (new StructuredName)
        .setProperty( "displayName", "Jim Smith" )

      val pairs = ContactData.structuredNames.dataPairs( name )
      val dataKeys = pairs.map{ _._1 }.filter{ _.startsWith("data") }

      dataKeys.toSeq should equal (Seq( "data1" ))

      // Make sure we have other fields.
      pairs.map{ _._1 } should contain ("raw_contact_id")
    }

    it ("should send all structured name fields if any are set") {
      val name = (new StructuredName)
        .setProperty( "displayName", "Jim Smith" )
        .setProperty( "givenName", "Jim" )
        .setProperty( "familyName", "Smith" )

      name.displayName should equal ("Jim Smith")
      name.givenName should equal ("Jim")

      val pairs = ContactData.structuredNames.dataPairs( name )
      val dataKeys = pairs.map{ _._1 }.filter{ _.startsWith("data") }

      dataKeys should contain ("data1")
      dataKeys should contain ("data2") // first name; set
      dataKeys should contain ("data7") // phonetic foo; unset
      pairs.map{ _._1 } should contain ("raw_contact_id") 
    }
  }
}
