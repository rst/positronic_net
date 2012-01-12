package org.positronicnet.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.util.ReflectUtils

class BaseClassForThing {
  val inheritedVar = "inherited"
}

class ThingWithNiladicConstructor extends BaseClassForThing {
  val instanceVar = "initialized"
}

case class ThingWithDefaultingConstructor(
  val x: String = "x",
  val y: Int    = 12345
)

class ThingWithLazyFields {
  lazy val fortyTwo   = 42
  val fiftyThree      = 53
  lazy val dog        = "grommit"
  val food            = "cheese"
}

case class ThingWithChangingDefaults( val fubar: Int = DefaultSequence.next )
object DefaultSequence {
  private var current = 0
  def next = {
    current = current + 1
    current
  }
}

class ReflectUtilsSpec
  extends Spec with ShouldMatchers
{
  describe( "declared fields by name" ) {
    it ("should get immediately declared fields") {
      val klass = classOf[ ThingWithDefaultingConstructor ]
      val fields = ReflectUtils.declaredFieldsByName( klass )
      fields( "x" ) should equal (klass.getDeclaredField( "x" ))
      fields( "y" ) should equal (klass.getDeclaredField( "y" ))
    }
    it ("should get inherited fields") {

      val baseKlass = classOf[ BaseClassForThing ]
      val klass     = classOf[ ThingWithNiladicConstructor ]
      val fields    = ReflectUtils.declaredFieldsByName( klass )

      fields("instanceVar")  should equal(klass.getDeclaredField("instanceVar"))
      fields("inheritedVar") should equal(baseKlass.getDeclaredField("inheritedVar"))
    }
  }

  describe( "get object builder" ) {

    it ("should find niladic constructor") {

      val builder = ReflectUtils.getObjectBuilder[ ThingWithNiladicConstructor ]
      val rec: ThingWithNiladicConstructor = builder()
      
      rec.inheritedVar should equal ("inherited")
      rec.instanceVar  should equal ("initialized")
    }

    describe("all-defaults constructor") {

      it ("should build objects correctly") {

        val builder =
          ReflectUtils.getObjectBuilder[ ThingWithDefaultingConstructor ]
        val rec: ThingWithDefaultingConstructor = builder()

        rec.x should equal ("x")
        rec.y should equal (12345)
      }

      it ("should invoke constructor arguments every time") {
        val builder = ReflectUtils.getObjectBuilder[ ThingWithChangingDefaults ]
        val x = builder()
        val y = builder()
        x.fubar should not equal (y.fubar)
      }
    }
  }

  describe( "allVals extractor" ) {
    it ("should find the correctly-typed fields (and only those)") {
      val getter = ReflectUtils.extractor( classOf[ThingWithLazyFields], 
                                           classOf[String] )
      val vs = (getter.get)( new ThingWithLazyFields )
      vs should equal (Map( "dog" -> "grommit", "food" -> "cheese" ))
    }
    it ("should find nothing where no fields exist") {
      val getter = ReflectUtils.extractor( classOf[ThingWithLazyFields],
                                           classOf[java.io.File] )
      getter should equal (None)
    }
  }

  describe ("collect public static values") {

    def checkCalendarInts( calendarInts: String => Int ) = {
      calendarInts("AM")          should equal (java.util.Calendar.AM)
      calendarInts("JANUARY")     should equal (java.util.Calendar.JANUARY)
      calendarInts("DAY_OF_WEEK") should equal (java.util.Calendar.DAY_OF_WEEK)
      calendarInts("FRIDAY")      should equal (java.util.Calendar.FRIDAY)
    }

    describe( "from class objects" ) {
      it ("should collect field values from Calendar") {
        val map =
          ReflectUtils.publicStaticValues( java.lang.Integer.TYPE,
                                           classOf[ java.util.Calendar] )
        checkCalendarInts( map(_) )
      }
    }

    describe( "from class manifests" ) {
      it ("should collect field values from Calendar") {
        // Reflection gets confused here about primitives.  I can live with it.
        val map = ReflectUtils.getStatics[ Int, java.util.Calendar ]
        checkCalendarInts( map(_).intValue )
      }

      it ("should handle redefinition of inherited statics") {

        // Free test fixtures in the Android SDK!  Guess how I discovered
        // this was an issue?

        import android.provider.ContactsContract.CommonDataKinds.StructuredName

        val strings = ReflectUtils.getStatics[ String, StructuredName ]
        strings( "DISPLAY_NAME" ) should be ("data1")

      }
    }
  }

  describe ("get single public static value") {
    it ("should get the value if available") {
      val friday = ReflectUtils.getStatic[ Int, java.util.Calendar ]("FRIDAY") 
      friday should equal (java.util.Calendar.FRIDAY)
    }
    it ("should signal on wrong type") {
      val exc = intercept[ RuntimeException ]{
        ReflectUtils.getStatic[ String, java.util.Calendar ]("FRIDAY")
      }
      exc.getMessage should (
        include ("String") and include ("Calendar") and include ("FRIDAY"))
    }
    it ("should signal on bad field") {
      val exc = intercept[ NoSuchFieldException ]{
        ReflectUtils.getStatic[ String, java.util.Calendar ]("FREAKY_FRIDAY")
      }
      exc.getMessage should include ("FREAKY")
    }
  }
}
