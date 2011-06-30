import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import org.positronic.pubsub.ChangeNotifier

class Specs extends Spec with ShouldMatchers {

  describe("change notifications") {

    val cd = new ChangeNotifier
    var callsA = 0
    var callsB = 0

    it( "should fire one listener" ) {
      cd.onChange( "A" ) { callsA += 1 }
      cd.noteChange

      callsA should be (1)
      callsB should be (0)
    }

    it( "should fire two listeners" ) {
      cd.onChange( "B" ) { callsB += 1 }
      cd.noteChange

      callsA should be (2)
      callsB should be (1)
    }

    it( "should be able to remove listeners" ) { 
      cd.stopChangeNotifications( "A" )
      cd.noteChange

      callsA should be (2)
      callsB should be (2)
    }
  }
}
