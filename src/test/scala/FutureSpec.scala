package org.positronicnet.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.notifications.Future

class FutureSpec
  extends Spec 
  with ShouldMatchers
  with RobolectricTests
{
  describe( "a future" ) {
    describe( "should forward success" ) {
      it( "... after handler attachment" ) {
        var result: Int = -1
        val f = new Future[ Int ]
        f.onSuccess{ result = _ }
        f.succeed( 42 )
        result should equal (42)
      }
      it( "... before handler attachment" ) {
        var result: Int = -1
        val f = new Future[ Int ]
        f.succeed( 42 )
        f.onSuccess{ result = _ }
        result should equal (42)
      }
    }
    describe( "should handle failure" ) {
      it( "... after handler attachment" ) {
        var result: Throwable = null
        val f = new Future[ Int ]
        f.onFailure{ result = _ }
        f.fail( new RuntimeException( "clogged oil filter" ))
        result should not equal (null)
        result.getMessage should equal ("clogged oil filter")
      }
      it( "... before handler attachment" ) {
        var result: Throwable = null
        val f = new Future[ Int ]
        f.fail( new RuntimeException( "clogged oil filter" ))
        f.onFailure{ result = _ }
        result should not equal (null)
        result.getMessage should equal ("clogged oil filter")
      }
    }
    describe( "monadic combinators" ) {
      describe( "map" ) {
        it ("should handle success") {
          val f1 = new Future[Int]
          val f2 = f1.map { _.toString }
          var result: String = null
          f2.onSuccess{ result = _ }
          f1.succeed( 42 )
          result should equal ("42")
        }
        it ("should handle failure in the mapped future") {
          val f1 = new Future[Int]
          val f2 = f1.map { _.toString }
          var result: Throwable = null
          f2.onFailure{ result = _ }
          f1.fail( new RuntimeException ("broken taillight") )
          result should not equal (null)
          result.getMessage should equal ("broken taillight")
        }
        it ("should handle exceptions thrown by the mapping function") {
          val f1 = new Future[Int]
          val func: (Int => String) = 
            ((x) => throw new RuntimeException("cracked manifold"))
          val f2 = f1.map { func }
          var result: Throwable = null
          f1.succeed( 42 )
          f2.onFailure{ result = _ }
          result should not equal (null)
          result.getMessage should equal ("cracked manifold")
        }
      }
      describe( "flatmap" ) {
        it ("should handle success") {
          val f1 = new Future[Int]
          val f2 = f1.flatMap{ i => {
            val fstring = new Future[String]
            fstring.succeed( i.toString )
            fstring
          }}
          var result: String = null
          f2.onSuccess{ result = _ }
          f1.succeed( 42 )
          result should equal ("42")
        }
        it ("should handle failure in the mapped future") {
          val f1 = new Future[Int]
          val f2 = f1.flatMap{ i => {
            val fstring = new Future[String]
            fstring.succeed( i.toString )
            fstring
          }}
          var result: Throwable = null
          f2.onFailure{ result = _ }
          f1.fail( new RuntimeException ("broken headlight") )
          result should not equal (null)
          result.getMessage should equal ("broken headlight")
        }
        it ("should handle failures in mapping function") {
          val f1 = new Future[Int]
          val f2 = f1.flatMap{ i => {
            val fstring = new Future[String]
            fstring.fail( new RuntimeException("burning oil") )
            fstring
          }}
          var result: Throwable = null
          f2.onFailure{ result = _ }
          f1.succeed( 42 )
          result should not equal (null)
          result.getMessage should equal ("burning oil")
        }
      }
    }
    describe("force") {
      it ("should handle success") {
        val f1 = Future(32)
        f1.force should be (32)
      }
      it ("should handle failure") {
        val f1 = new Future[Int]
        f1.fail( new RuntimeException( "leaking transmission fluid" ))
        val exc = intercept[ RuntimeException ]{ f1.force }
        exc.getMessage should be ("Exception completing future")
        exc.getCause.getMessage should be ("leaking transmission fluid")
      }
    }
  }
  describe ("future companion object") {
    it ("should allow lifting values into futures") {
      val x = Future( 32 )
      var i = -1
      x.onSuccess{ i = _ }
      i should be (32)
    }
    it ("should trasumute a collection of futures to a future of a collection"){
      val futures = Seq( Future(3), Future(4), Future(5) )
      val future = Future.sequence( futures )
      var x: Seq[Int] = Seq.empty
      future.onSuccess{ x = _ }
      x should equal (Seq( 3, 4, 5 ))
    }
  }
}

