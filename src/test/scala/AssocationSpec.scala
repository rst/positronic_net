package org.positronicnet.test

import org.positronicnet.db._
import org.positronicnet.orm._
import org.positronicnet.util.AddWatcherAndFetch

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class AssociationSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  var dogList: TodoList = null
  var catList: TodoList = null

  override def beforeEach = {
    db.setupFixturesForAssociationTest
    val lseq = TodoLists.fetchOnThisThread
    dogList = lseq.find{ _.name == "dog list" }.get
    catList = lseq.find{ _.name == "cat list" }.get
  }

  describe( "has many association" ) {

    it( "should find only relevant records" ) {
      TodoItems.count.fetchOnThisThread should equal(4)
      dogList.items.count.fetchOnThisThread should equal(3)
      assert( dogList.items.fetchOnThisThread.forall { item => 
          item.todoListId == dogList.id })
    }

    it( "should propagate change notifications across copies" ) {

      // Set up two copies of a TodoList, with two distinct 'items'
      // association proxies.  Monitor count on one, update the other,
      // and verify that the change propagates appropriately.

      val dogListCopy = dogList.copy()
      var monitoredCount: Long = -457     // clearly invalid...

      dogList.items.count.watch( this ){ count => { monitoredCount = count }}
      
      dogListCopy.items.onThisThread( DeleteAll( TodoItem() ))

      monitoredCount should equal (0)

      dogList.items.count.stopNotifier( this )
    }

    it ( "should have a working create()" ) {
      dogList.items.create should equal ( TodoItem( todoListId = dogList.id ))
    }
  }
}
