import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import org.positronicnet.orm._

class Place 
  extends Record 
{
  lazy val latitude  = longField( "latitude", this )
  lazy val longitude = longField( "longitude", this )
}

object Place extends Place with RecordManager

class Specs extends Spec with ShouldMatchers {
  describe("orm basics") {
    it("should support basic accessor/setter semantics") {

      val z = new Place
      val y = z.latitude( 44 )
      val x = y.longitude( 53 )
      
      x.getClass  should equal (classOf[ Place ])

      x.latitude.value  should equal (44)
      x.longitude.value should equal (53)

      y.latitude.optValue  should equal ( Some( 44 ))
      y.longitude.optValue should equal ( None )
    }
  }
}
