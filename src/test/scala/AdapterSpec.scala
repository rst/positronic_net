package org.positronicnet.ui.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import org.positronicnet.ui._

import com.xtremelabs.robolectric.Robolectric
import org.positronicnet.test.RobolectricTests

class AdapterSpec
  extends Spec
  with ShouldMatchers
  with RobolectricTests
{
  describe("IndexedSeqGroupAdapter") {

    val data = 
      IndexedSeq( ("Planes",      IndexedSeq( "SR-71", "B747" )),
                  ("Trains",      IndexedSeq( "GG-1",  "Acela", "TGV" )),
                  ("Automobiles", IndexedSeq( "Stutz Bearcat", "Tesla S")))

    def adapter = new IndexedSeqGroupAdapter( data, -1, -1 )

    it ("should report counts correctly") {
      adapter.getGroupCount should equal (3)
      adapter.getChildrenCount (0) should equal (2)
      adapter.getChildrenCount (1) should equal (3)
    }

    it ("should fake ids correctly") {
      adapter.getGroupId(1)   should equal (1)
      adapter.getGroupId(2)   should equal (2)
      adapter.getChildId(0,1) should equal (1)
      adapter.getChildId(0,2) should equal (2)
      adapter.getChildId(1,1) should equal (1)
      adapter.getChildId(1,2) should equal (2)
    }

    it ("should retrieve items correctly") {
      adapter.getGroup(0)   should equal ("Planes")
      adapter.getChild(0,0) should equal ("SR-71")
    }
  }
}
