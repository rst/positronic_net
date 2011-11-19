package org.positronicnet.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.util.Lensable
import org.positronicnet.util.LensImpl
import org.positronicnet.util.LensFactory

case class Canary( intProp: Int = 17,
                   stringProp: String = "foo",
                   massagedIntInner: Int = -4,
                   massagedStringInner: String = "::glorp",
                   otherThing: String = ""
                 )
  extends Lensable
{
  // Define some pseudoproperties, so we can test how they're handled.

  def massagedInt = -massagedIntInner
  def massagedInt_:=( x: Int ) = copy( massagedIntInner = -x )

  def massagedString = massagedStringInner.slice(2, 1000)
  def massagedString_:=( s: String ) = copy( massagedStringInner = "::"+s )
}

class LensSpec
  extends Spec with ShouldMatchers
{
  def testLens[V]( l: LensImpl[Canary,V], defaultVal: V, otherVal: V ) = {

    val testCanary = Canary( otherThing = "coalmine" )

    l.getter( testCanary ) should equal ( defaultVal )

    val setCanary = l.setter( testCanary, otherVal )
    
    l.getter( testCanary ) should equal ( defaultVal )
    l.getter( setCanary )  should equal ( otherVal )
    setCanary.otherThing   should equal ( "coalmine" )
  }

  describe( "int lens factory" ) {

    val fac: LensFactory[ Int ] = LensFactory.forType[ Int ]

    it ("should work for plain int fields") {
      testLens( fac.implForProperty[ Canary ]( "intProp" ).get, 17, 42 )
    }
    it ("should work for wrapped int fields") {
      testLens( fac.implForProperty[ Canary ]( "massagedInt" ).get, 4, 8 )
    }
    it ("should not find fields of the wrong type") {
      fac.implForProperty[ Canary ]( "stringProp" ) should equal (None)
      fac.implForProperty[ Canary ]( "massagedString" ) should equal (None)
      fac.implForProperty[ Canary ]( "quux" ) should equal (None)
    }
  }

  describe( "string lens factory" ) {

    val fac: LensFactory[ String ] = LensFactory.forType[ String ]

    it ("should work for plain string fields") {
      testLens( fac.implForProperty[ Canary ]( "stringProp" ).get, "foo", "bar" )
    }
    it ("should work for wrapped int fields") {
      testLens( fac.implForProperty[ Canary ]( "massagedString" ).get, "glorp", "gleep" )
    }
    it ("should not find fields of the wrong type") {
      fac.implForProperty[ Canary ]( "intProp" ) should equal (None)
      fac.implForProperty[ Canary ]( "massagedInt" ) should equal (None)
      fac.implForProperty[ Canary ]( "quux" ) should equal (None)
    }
  }

}
