package org.positronicnet.orm.variantTest

import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.db._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.positronicnet.test.RobolectricTests
import com.xtremelabs.robolectric.Robolectric

// Simple single-template inheritance schema

object PeopleDb 
  extends Database( filename="people", logTag="people" )
{
  def schemaUpdates = 
    List( """
          create table people (
            _id int identity,
            person_type varchar(100),
            name varchar(100),
            class_year int,
            rating int
          )
          """ )

  def setupFixtures = {
    this("people").delete
    this("people").insert ("person_type" -> "student",
                           "name"        -> "Charlie Brown",
                           "class_year"  -> 2011)
    this("people").insert ("person_type" -> "student",
                           "name"        -> "Sally Brown",
                           "class_year"  -> 2014)
    this("people").insert ("person_type" -> "student",
                           "name"        -> "Lucy van Pelt",
                           "class_year"  -> 2011)
    this("people").insert ("person_type" -> "student",
                           "name"        -> "Linus van Pelt",
                           "class_year"  -> 2014)
    this("people").insert ("person_type" -> "teacher",
                           "name"        -> "Mwom wom wom Mwom",
                           "rating"      -> 6)
  }
}

abstract class Person extends ManagedRecord {
  val name: String
} 

case class Student (
  val name: String = null,
  val classYear: Int = -1,
  val id : RecordId[Student] = People.students.unsavedId
)
extends Person

case class Teacher (
  val name: String = null,
  val rating: Int = -1,
  val id : RecordId[Teacher] = People.teachers.unsavedId
)
extends Person

object People
  extends VariantRecordManager[Person]( PeopleDb( "people" ), "person_type")
{
  val students = new TaggedVariant[ Student ]("student")
  val teachers = new TaggedVariant[ Teacher ]("teacher")
}

// The actual spec

class FieldMappingSpec
  extends Spec 
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
{
  override def beforeEach = {
    PeopleDb.openInContext( Robolectric.application )
    PeopleDb.setupFixtures
  }

  override def afterEach  = PeopleDb.close

  describe( "field mapping in variants" ) {

    // We're in a subpackage of ORM, so we have access to internals...

    it ("should collect all fields in base mgr") {
      People.allBaseCursorColumns.sorted.toList should equal (
        List( "_id", "class_year", "name", "person_type", "rating" ))
    }

    it ("should have correct indexes for subtype fields in generic queries") {
      val baseCols = People.allBaseCursorColumns
      for (subMgr <- Seq( People.students, People.teachers )) {
        subMgr.fieldsFromParentQuery.size should equal (subMgr.fields.size)
        for (fld <- subMgr.fieldsFromParentQuery) {
          fld.realColNumber should equal (baseCols.indexOf (fld.dbColumnName))
        }
      }
    }
  }

  describe( "a single variant manager" ) {

    it ("should find pre-placed records") {
      val students = People.students.order("name").fetchOnThisThread
      students should have size (4)
      students.map{_.name} should equal (
        Seq("Charlie Brown", "Linus van Pelt", "Lucy van Pelt", "Sally Brown"))
      students.map{_.classYear} should equal (Seq(2011, 2014, 2011, 2014))
    }

    it ("should be able to insert") {
      People.students.onThisThread (Save (Student ("Pigpen", 2010)))
      People.students.count.fetchOnThisThread should equal (5)

      val pigpens = People.students.whereEq("name"->"Pigpen").fetchOnThisThread
      pigpens should have size (1)

      val pigpen = pigpens(0)
      pigpen.name should equal ("Pigpen")
      pigpen.classYear should equal (2010)
    }

    it ("should be able to update") {
      val lucy = 
        (People.students.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      People.students.onThisThread (Save (lucy.copy (classYear = 2010)))

      val nucy = 
        (People.students.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      nucy.classYear should equal (2010)
    }

    it ("should be able to delete") {
      val lucy = 
        (People.students.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      People.students.onThisThread (Delete (lucy))

      val students = People.students.order("name").fetchOnThisThread
      students should have size (3)
      students.map{_.name} should equal (
        Seq("Charlie Brown", "Linus van Pelt", "Sally Brown"))
    }

  }
}
