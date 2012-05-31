package org.positronicnet.content.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.content._

import com.xtremelabs.robolectric.Robolectric
import org.positronicnet.test.RobolectricTests

import android.net.Uri

class UriMatcherSpec 
  extends Spec 
  with ShouldMatchers
  with RobolectricTests
{
  def assertMatch( matcher: UriMatcher[String],
                   uriStr: String, 
                   expectTag: String,
                   expectVals: ContentValue* ) =
  {
    val option = matcher.withMatchOption( Uri.parse( uriStr )){ (tag, vals) =>
      tag should be (expectTag)
      vals.size should be (expectVals.size)
      for ( pair <- vals.zip( expectVals )) {
        pair._1.asConditionString should be (pair._2.asConditionString)
      }
    }
    option should not be (None)
  }

  def assertNoMatch( matcher: UriMatcher[String], uriStr: String ) = {
    val option = matcher.withMatchOption( Uri.parse( uriStr )){ (tag, vals) =>
      "foo"
    }
    option should be (None)
  }

  describe( "simple matches" ) {

    object SimpleMatcher extends UriMatcher[String] {
      matchUri( "content://org.testa/foo/bar", "a:foobar" )
      matchUri( "content://org.testb/foo/bar", "b:foobar" )
      matchUri( "content://org.testb/foo/bxx", "b:foobxx" )
    }

    it ("should discriminate on segments") {
      assertMatch( SimpleMatcher, "content://org.testb/foo/bar", "b:foobar" )
      assertMatch( SimpleMatcher, "content://org.testb/foo/bxx", "b:foobxx" )
    }

    it ("should discriminate on authority portions") {
      assertMatch( SimpleMatcher, "content://org.testa/foo/bar", "a:foobar" )
      assertMatch( SimpleMatcher, "content://org.testb/foo/bar", "b:foobar" )
    }

    it ("should not match prefixes") {
      assertNoMatch( SimpleMatcher, "content://org.testa/foo" )
    }

    it ("should not match URIs with extra segments") {
      assertNoMatch( SimpleMatcher, "content://org.testa/foo/bar/baz" )
    }
  }

  describe( "matches with string wildcard" ) {

    object StringWildMatcher extends UriMatcher[String] {
      matchUri( "content://org.test/foo/*", "nosuffix" )
      matchUri( "content://org.test/foo/*/bar", "suffixbar" )
      matchUri( "content://org.test/foo/*/baz", "suffixbaz" )
    }

    it ("should not match no segment") {
      assertNoMatch( StringWildMatcher, "content://org.test/foo" )
    }

    it ("should match without suffixes") {
      assertMatch( StringWildMatcher, "content://org.test/foo/krugman", 
                   "nosuffix", "krugman" )
    }

    it ("should match correct suffix") {
      assertMatch( StringWildMatcher, "content://org.test/foo/stiglitz/bar", 
                   "suffixbar", "stiglitz" )
      assertMatch( StringWildMatcher, "content://org.test/foo/stiglitz/baz", 
                   "suffixbaz", "stiglitz" )
    }

    it ("should not match random unspecified suffix") {
      assertNoMatch( StringWildMatcher, "content://org.test/foo/x/x" )
    }
  }

  describe( "matches with numeric wildcard" ) {

    // Testing only what's different from the string wildcard case;
    // most of the code is on a common base class, and there's no need
    // to test it twice.

    object NumericWildMatcher extends UriMatcher[String] {
      matchUri( "content://org.test/foo/=", "nmatch" )
    }

    it ("should correctly match an integer") {
      assertMatch( NumericWildMatcher, "content://org.test/foo/3232",
                   "nmatch", 3232L )
    }
    
    it ("should not match with special characters") {
      assertNoMatch( NumericWildMatcher, "content://org.test/foo/32.32" )
    }
    
  }
}
