package org.positronicnet.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.util.ReflectiveProperties
import org.positronicnet.util.Lens
import org.positronicnet.util.LensFactory

case class Canary( intProp: Int = 17,
                   byteProp: Byte = 8,
                   charProp: Char = 10,
                   shortProp: Short = 30001,
                   longProp: Long = (1L << 60),
                   stringProp: String = "foo",
                   floatProp: Float = 3.5f,
                   doubleProp: Double = 1e200,
                   booleanProp: Boolean = true,

                   massagedIntInner: Int = -4,
                   massagedByteInner: Byte = 7,
                   massagedCharInner: Char = 9,
                   massagedShortInner: Short = 30000,
                   massagedLongInner: Long = (1L << 60) - 1,
                   massagedFloatInner: Float = 7f,
                   massagedDoubleInner: Double = 2e200,
                   massagedBooleanInner: Boolean = false,
                   massagedStringInner: String = "::glorp",

                   otherThing: String = ""
                 )
  extends ReflectiveProperties
{
  // Define some pseudoproperties, so we can test how they're handled.
  // The intent of this feature is to support cases where, say, a Date
  // (as far as the UI is concerned) is stored as a Long internally.
  //
  // Here, the underlying storage in all cases is of the same type,
  // but transformed somehow to keep the machinery honest.  (Slight
  // subtlety:  the float types are transformed by multiplication or
  // division by two, because these are exact operations on floats;
  // they're integer math on the exponent.)

  def massagedInt = -massagedIntInner
  def massagedInt_:=( x: Int ) = copy( massagedIntInner = -x )

  def massagedByte = (massagedByteInner + 1).asInstanceOf[ Byte ]
  def massagedByte_:=( x: Byte ) = 
    copy( massagedByteInner = (x-1).asInstanceOf[Byte] )

  def massagedChar = (massagedCharInner + 1).asInstanceOf[ Char ]
  def massagedChar_:=( x: Char ) = 
    copy( massagedCharInner = (x-1).asInstanceOf[Char] )

  def massagedShort = (massagedShortInner + 1).asInstanceOf[ Short ]
  def massagedShort_:=( x: Short ) = 
    copy( massagedShortInner = (x-1).asInstanceOf[Short] )

  def massagedLong = (massagedLongInner + 1).asInstanceOf[ Long ]
  def massagedLong_:=( x: Long ) = 
    copy( massagedLongInner = (x-1).asInstanceOf[Long] )

  def massagedFloat = massagedFloatInner / 2.0f
  def massagedFloat_:=( x: Float ) = copy( massagedFloatInner = x * 2.0f )

  def massagedDouble = massagedDoubleInner / 2.0
  def massagedDouble_:=( x: Double ) = copy( massagedDoubleInner = x * 2.0 )

  def massagedBoolean = !massagedBooleanInner
  def massagedBoolean_:=( x: Boolean ) = copy( massagedBooleanInner = !x )

  def massagedString = massagedStringInner.slice(2, 1000)
  def massagedString_:=( s: String ) = copy( massagedStringInner = "::"+s )
}

class ReflectivePropertiesSpec
  extends Spec with ShouldMatchers
{
  def testLens[V]( l: Lens[Canary,V], defaultVal: V, otherVal: V ) = {

    val testCanary = Canary( otherThing = "coalmine" )

    l.getter( testCanary ) should equal ( defaultVal )

    val setCanary = l.setter( testCanary, otherVal )
    
    l.getter( testCanary ) should equal ( defaultVal )
    l.getter( setCanary )  should equal ( otherVal )
    setCanary.otherThing   should equal ( "coalmine" )
  }

  def testPropApi[V:ClassManifest]( propName: String, defaultVal: V, otherVal: V ) = {
    
    val testCanary = Canary( otherThing = "coalmine" )

    testCanary.getProperty[V]( propName ) should equal (defaultVal)

    val setCanary = testCanary.setProperty[V]( propName, otherVal )
    
    testCanary.getProperty[V]( propName )     should equal ( defaultVal )
    setCanary.getProperty[V](  propName )     should equal ( otherVal )
    setCanary.asInstanceOf[Canary].otherThing should equal ( "coalmine" )
  }

  def testProperty[V:ClassManifest]( factory: LensFactory[V], propName: String, 
                                     defaultVal: V, otherVal: V ) = {
    testPropApi( propName, defaultVal, otherVal )
    testLens( factory.forProperty[ Canary ]( propName ).get, 
              defaultVal, otherVal )
  }

  describe( "int lens factory" ) {

    val fac: LensFactory[ Int ] = LensFactory.forPropertyType[ Int ]

    it ("should work for plain int fields") {
      testProperty( fac, "intProp", 17, 42 )
    }
    it ("should work for wrapped int fields") {
      testProperty( fac, "massagedInt", 4, 8 )
    }
    it ("should not find fields of the wrong type") {
      fac.forProperty[ Canary ]( "stringProp" ) should equal (None)
      fac.forProperty[ Canary ]( "massagedString" ) should equal (None)
      fac.forProperty[ Canary ]( "quux" ) should equal (None)
    }
  }

  describe( "string lens factory" ) {

    val fac: LensFactory[ String ] = LensFactory.forPropertyType[ String ]

    it ("should work for plain string fields") {
      testProperty( fac, "stringProp", "foo", "bar" )
    }
    it ("should work for wrapped int fields") {
      testProperty( fac, "massagedString", "glorp", "gleep" )
    }
    it ("should not find fields of the wrong type") {
      fac.forProperty[ Canary ]( "intProp" ) should equal (None)
      fac.forProperty[ Canary ]( "massagedInt" ) should equal (None)
      fac.forProperty[ Canary ]( "quux" ) should equal (None)
    }
  }

  describe( "byte lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Byte ]
      val default: Byte = 8
      val newVal:  Byte = 12
      testProperty( fac, "byteProp",     default, newVal )
      testProperty( fac, "massagedByte", default, newVal )
    }
  }

  describe( "char lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Char ]
      val default: Char = 10
      val newVal:  Char = 15
      testProperty( fac, "charProp",     default, newVal )
      testProperty( fac, "massagedChar", default, newVal )
    }
  }
 
  describe( "short lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Short ]
      val default: Short = 30001
      val newVal:  Short = 15
      testProperty( fac, "shortProp",     default, newVal )
      testProperty( fac, "massagedShort", default, newVal )
    }
  }

  describe( "long lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Long ]
      val default: Long = (1L << 60)
      val newVal:  Long = 15
      testProperty( fac, "longProp",     default, newVal )
      testProperty( fac, "massagedLong", default, newVal )
    }
  }

  describe( "float lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Float ]
      val default: Float = 3.5f
      val newVal:  Float = 14f
      testProperty( fac, "floatProp",     default, newVal )
      testProperty( fac, "massagedFloat", default, newVal )
    }
  }

  describe( "double lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Double ]
      val default: Double = 1e200
      val newVal:  Double = 7
      testProperty( fac, "doubleProp",     default, newVal )
      testProperty( fac, "massagedDouble", default, newVal )
    }
  }

  describe( "boolean lens factory" ) {
    it ("should handle things properly") {
      val fac = LensFactory.forPropertyType[ Boolean ]
      val default: Boolean = true
      val newVal:  Boolean = false
      testProperty( fac, "booleanProp",     default, newVal )
      testProperty( fac, "massagedBoolean", default, newVal )
    }
  }
}
