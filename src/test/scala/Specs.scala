import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import org.positronic.pubsub.ChangeNotifications

class Specs extends Spec with ShouldMatchers {

  class ChangeDummy extends ChangeNotifications

  describe("change notifications") {

    val cd = new ChangeDummy
    var callsA = 0
    var callsB = 0

    it( "should fire one" ) {
      cd.onChange( "A" ) { callsA += 1 }
      cd.noteChange

      callsA should be (1)
      callsB should be (0)
    }

    it( "should fire two" ) {
      cd.onChange( "B" ) { callsB += 1 }
      cd.noteChange

      callsA should be (2)
      callsB should be (1)
    }

    it( "should be able to remove notifiers" ) { 
      cd.stopChangeNotifications( "A" )
      cd.noteChange

      callsA should be (2)
      callsB should be (2)
    }
  }
}
