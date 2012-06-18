package org.positronicnet.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.notifications._

import scala.collection.mutable.ArrayBuffer

class TestDataStream[ T ] extends ExplicitNotificationDataStream[ T ]
{
  def send(s: T) = noteNewValue(s)
}

class TestDuration 
  extends Duration 
  with ExplicitNotificationDataStream[ DurationEvent ]
{
  def start = noteNewValue( DurationStart )
  def stop  = noteNewValue( DurationStop )
}

class DataStreamSpec
  extends Spec
  with ShouldMatchers
  with RobolectricTests
{
  def valBuffer[T]( stream: DataStream[T] ) ={
    val values = new ArrayBuffer[T]
    stream.withValues { values += _ }
    values
  }

  describe( "plain data stream, no combinators" ) {
    it ("should report values pushed after we start listening") {
      val stream = new TestDataStream[ String ]
      val buf = valBuffer( stream )
      stream.send( "foo" ); stream.send( "bar" ); stream.send( "moo" )
      buf.toSeq should be (Seq( "foo", "bar", "moo" ))
    }
    it ("should report current value as of when we start listening") {
      val stream = new TestDataStream[ String ]

      stream.send( "foo" ); stream.send( "bar" )

      val buf = valBuffer( stream )

      stream.send( "baz" )
      buf.toSeq should be (Seq( "bar", "baz" ))
    }
    it ("should allow listeners to disengage") {
      val stream = new TestDataStream[ String ]
      val dummyTag = new Object
      val buf = new ArrayBuffer[String]
      
      stream.send( "foo" ); stream.send( "bar" )
      
      stream.addListener( dummyTag, buf += _ )

      stream.send( "moo" ); stream.send( "baz" )

      stream.removeListener( dummyTag )

      stream.send( "blurfl" )

      buf.toSeq should be (Seq( "bar", "moo", "baz"))
    }
  }

  describe( "mapped data stream" ) {

    def streams = {
      val s = new TestDataStream[ String ]
      (s, s.map{ Integer.parseInt( _ ) })
    }

    it ("should report values pushed after we start listening") {
      val (stringStream, intStream) = streams
      val buf = valBuffer( intStream )

      stringStream.send("44"); stringStream.send("23"); stringStream.send("56")

      buf.toSeq should be (Seq( 44, 23, 56 ))
    }

    it ("should respect listeners cutting in and out") {
      val (stringStream, intStream) = streams

      stringStream.send("44"); stringStream.send("23")

      val buf = new ArrayBuffer[ Int ]
      val tag = new Object
      intStream.addListener( tag, buf += _ )

      stringStream.send("88"); stringStream.send("22")

      intStream.removeListener( tag )
      stringStream.send("1955")

      buf.toSeq should be (Seq( 23, 88, 22 ))
    }
  }

  describe( "predicate-filtered data stream" ) {

    it ("should include only values matching the predicate while listening") {

      val baseStream = new TestDataStream[ Int ]
      val oddStream = baseStream.filter{ _ % 2 == 1 }

      baseStream.send( 2 ); baseStream.send( 5 )
      val buf = new ArrayBuffer[ Int ]
      val tag = new Object
      oddStream.addListener( tag, buf += _ )

      baseStream.send( 4 ); baseStream.send( 7 ) 
      baseStream.send( 6 ); baseStream.send( 9 )

      oddStream.removeListener( tag )
      baseStream.send( 6 ); baseStream.send( 9 )

      buf.toSeq should be (Seq( 5, 7, 9 ))
    }
    
  }

  describe( "duration-filtered stream" ) {
    it ("should include only values sent while 'duration' is active") {

      val myDuration = new TestDuration
      val base = new TestDataStream[ String ]
      val filtered = base.during( myDuration )
      val buf = valBuffer( filtered )
      
      base.send( "off" ); base.send( "still off" )
      base.send( "transition" )

      myDuration.start

      base.send( "on" ); base.send( "still on" )

      myDuration.stop

      base.send( "back off" ); base.send( "transition" )

      myDuration.start

      base.send( "on again" )

      buf.toSeq should be (Seq( "transition", "on", "still on",
                                "transition", "on again" ))
    }
  }

  describe( "flatmapping of streams" ) {
    it ("should behave properly in a simple switching example") {

      val astream = new TestDataStream[ String ]
      val bstream = new TestDataStream[ String ]
      val cstream = new TestDataStream[ String ]

      def step = {
        astream.send("a"); bstream.send("b"); cstream.send("c")
      }

      val intStream = new TestDataStream[ Int ]
      val flatStream = intStream.flatMap{ _ match {
        case 1 => astream
        case 2 => bstream
        case 3 => cstream
      }}

      val buf = valBuffer( flatStream )

      step; step; step

      intStream.send( 1 )               // capture an a; set up for more
      step; step                        // two more a's
      intStream.send( 2 )               // capture a b
      step                              // one more b
      intStream.send( 3 )               // get a c

      buf.toSeq should be (Seq( "a", "a", "a", "b", "b", "c" ))
    }
  }
}
