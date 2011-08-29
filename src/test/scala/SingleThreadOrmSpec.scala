package org.positronicnet.test

import org.positronicnet.db._
import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.util._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class SingleThreadOrmSpec
  extends Spec 
  with ShouldMatchers
  with DbTestFixtures
{
  describe( "Single-thread ORM queries" ){
    it ("should find all the records") {
      val results = TodoItems.fetchOnThisThread
      results should have size (3)

      assert( haveItem( "wash dog" , false, results ))
      assert( haveItem( "feed dog" , false, results ))
      assert( haveItem( "walk dog" , true,  results ))
    }
    it ("should retrieve only matching records with conds"){
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread

      undoneItems should have size (2)
      assert( haveItem( "wash dog" , false, undoneItems ))
      assert( haveItem( "feed dog" , false, undoneItems ))
    }
  }

  describe( "Single-thread ORM count") {
    it ("should count all records with no conds") {
      TodoItems.count.fetchOnThisThread should equal (3)
    }
    it ("should count the right records with conds") {
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> true  ).count.fetchOnThisThread should equal (1)
    }
  }

  describe( "Single-thread ORM delete via scope" ) {
    it ("should eliminate selected records") {
      TodoItems.whereEq( "is_done" -> true ).onThisThread( DeleteAll )
      TodoItems.count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM delete via record selection" ) {
    it ("should kill the selected record") {
      val doneItems = TodoItems.whereEq( "is_done" -> true ).fetchOnThisThread
      doneItems should have size (1)

      doneItems.foreach{ TodoItems ! Delete( _ ) }

      TodoItems.count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM update via scope" ) {
    it ("should change counts") {
      TodoItems.whereEq( "description" -> "feed dog" ).onThisThread( UpdateAll("is_done" -> true))
      TodoItems.whereEq( "is_done" -> true ).count.fetchOnThisThread should equal (2)
    }
  }

  describe( "Single-thread ORM update via record selection" ) {

    def doUpdate = {
      val undoneItems = TodoItems.whereEq( "is_done" -> false).fetchOnThisThread
      undoneItems should have size (2)

      val doneItem = undoneItems( 0 ).isDone( true )
      TodoItems.onThisThread( Save( doneItem ))
                      
      doneItem
    }

    it ("should change counts") {
      doUpdate
      TodoItems.count.fetchOnThisThread should equal (3)
      TodoItems.whereEq( "is_done" -> true  ).count.fetchOnThisThread should equal (2)
      TodoItems.whereEq( "is_done" -> false ).count.fetchOnThisThread should equal (1)
    }
      
    it ("should alter the record") {
      val changedItem = doUpdate
      val doneItems = TodoItems.fetchOnThisThread
      assert( haveItem( changedItem.description, true, doneItems ))
    }
  }

  describe( "Single-thread ORM insert" ) {

    def doInsert = TodoItems.onThisThread( Save( TodoItem("train dog", false )))
    
    it ("should alter counts") {
      doInsert
      TodoItems.count.fetchOnThisThread should equal (4)
    }

    it ("should make the record show up on queries") {
      doInsert
      val items = TodoItems.fetchOnThisThread
      assert( haveItem( "train dog", false, items ))
      items.find{ _.description == "train dog" }.map{ 
        _.id should not equal( -1 )
      }
    }
  }

  describe( "ORM .order(...) subscopes" ) {
    it ("should give the right answer") {

      val ascItems = TodoItems.order( "description asc" ).fetchOnThisThread
      ascItems.map{ _.description }.toList should equal (
        List("feed dog","walk dog","wash dog"))

      val descItems = TodoItems.order( "description desc" ).fetchOnThisThread
      descItems.map{ _.description }.toList should equal (
        List("wash dog","walk dog","feed dog"))
    }
    it ("should propagate updates up") {
      val orderedItems = TodoItems.order( "description asc" )
      var monitoredCount: Long = -457
      orderedItems ! AddWatcher(this, items => { monitoredCount = items.size })
      TodoItems.onThisThread( Save( TodoItem( "crate dog" )))
      monitoredCount should equal (4)
      orderedItems ! StopWatching(this)
    }
    it ("should propagate updates down") {
      val orderedItems = TodoItems.order( "description asc" )
      var monitoredCount: Long = -457
      TodoItems ! AddWatcher(this, items => { monitoredCount = items.size })
      TodoItems.onThisThread( Save( TodoItem( "crate dog" )))
      monitoredCount should equal (4)
      TodoItems ! StopWatching( this )
    }
  }

  describe( "ORM .limit(...) subscopes" ) {
    it ("should give the right answer") {
      val ascScope = TodoItems.order( "description asc" ).limit(2)
      val ascItems = ascScope.fetchOnThisThread
      ascItems.map{ _.description }.toList should equal (
        List("feed dog","walk dog"))
    }
  }

  describe( "record query support" ) {
    it ("should be set right initially") {
      val qry = TodoItems.recordsQuery( "w%" ){ (s, q) => 
        q.where( "description like ?", Array(s) ) }
      val records: Seq[ TodoItem ] = qry.fetchOnThisThread
      val descs = records.map{_.description}.toList.sorted
      descs should equal( List( "walk dog", "wash dog" ))
    }
    it ("should be reset on requery") {
      val qry = TodoItems.recordsQuery( "w%" ){ (s, q) => 
        q.where( "description like ?", Array(s) ) }
      var records = qry.fetchOnThisThread
      qry.watch( this ){ records = _ }
      qry.onThisThread( Requery( "% dog" ))
      val descs = records.map{_.description}.toList.sorted
      descs should equal( List( "feed dog", "walk dog", "wash dog" ))
      qry.stopNotifier( this )
    }
  }

  describe( "count query support" ) {
    it ("should be set right initially") {

      val qry = TodoItems.countQuery( "w%" ){ (s, q) => 
        q.where( "description like ?", Array(s) ) }

      qry.fetchOnThisThread should equal (2)
    }
    it ("should be reset on requery") {

      val qry = TodoItems.countQuery( "w%" ){ (s, q) => 
        q.where( "description like ?", Array(s) ) }

      qry.onThisThread( Requery( "% dog" ))
      qry.fetchOnThisThread should equal (3)
    }
  }
}
