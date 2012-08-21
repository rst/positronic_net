package org.positronicnet.orm.variantTest

import org.positronicnet.orm._
import org.positronicnet.orm.Actions._
import org.positronicnet.db._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.positronicnet.test.{RobolectricTests,SerializationTestHelpers}
import com.xtremelabs.robolectric.Robolectric

// Simple single-template inheritance schema

object PeopleDb 
  extends Database( filename="people", logTag="people" )
{
  def schemaUpdates = 
    List( """
          create table schools (
            _id integer primary key,
            name string
          )
          """,

          """
          create table people (
            _id integer primary key,
            person_type string,
            school_id integer,
            name string,
            class_year integer,
            rating integer
          )
          """ )

  // Populate the schema using the raw(er) positronicnet.db API directly.
  // (Except for the monomorphic schools table, where I'll permit myself
  // to assume here that a simple RecordManager works, that being tested
  // elsewhere.)

  def setupFixtures = {
    val squery = this("schools")
    squery.delete
    squery.insert ("name" -> "Rod Blagojevich Junior Academy")
    squery.insert ("name" -> "Teapot Dome Elementary School")

    val schools = Schools.order("name").fetchOnThisThread
    blagoSchoolId = (Schools.fetchOnThisThread)(0).id
    teapoSchoolId = (Schools.fetchOnThisThread)(1).id

    val pquery = this("people")
    pquery.delete
    pquery.insert ("person_type" -> "student",
                   "name"        -> "Charlie Brown",
                   "school_id"   -> blagoSchoolId,
                   "class_year"  -> 2011)
    pquery.insert ("person_type" -> "student",
                   "name"        -> "Sally Brown",
                   "school_id"   -> teapoSchoolId,
                   "class_year"  -> 2014)
    pquery.insert ("person_type" -> "student",
                   "name"        -> "Lucy van Pelt",
                   "school_id"   -> blagoSchoolId,
                   "class_year"  -> 2011)
    pquery.insert ("person_type" -> "student",
                   "name"        -> "Linus van Pelt",
                   "school_id"   -> teapoSchoolId,
                   "class_year"  -> 2014)
    pquery.insert ("person_type" -> "teacher",
                   "name"        -> "Mwom wom wom Mwom",
                   "school_id"   -> blagoSchoolId,
                   "rating"      -> 6)
    pquery.insert ("person_type" -> "beagle",
                   "name"        -> "Snoopy")

    numPeople = pquery.count.asInstanceOf[Int]
    peopleNames = pquery.order("name").select("name").map{_.getString(0)}

    val studentQuery = pquery.whereEq("person_type"->"student")
    numStudents = studentQuery.count.asInstanceOf[Int]
    studentNames = studentQuery.order("name").select("name").map{_.getString(0)}
  }

  var blagoSchoolId: RecordId[School] = null
  var teapoSchoolId: RecordId[School] = null

  var numPeople = 0                // reset by setupFixtures
  var peopleNames = Seq ("reset by setupFixtures") 

  var numStudents = 0              // reset by setupFixtures
  var studentNames = Seq ("reset by setupFixtures") 
}

// Simple "school" model, so our polymorphic people have something to
// associate with...

case class School (
  val name: String = null,
  val id: RecordId[School] = Schools.unsavedId
)
extends ManagedRecord
{
  lazy val people = new HasMany( People, "school_id" ) // XXX shouldn't need col name
  lazy val students = new HasMany( People.students )
}

object Schools extends RecordManager[School]( PeopleDb("schools"))

// People model(s)

abstract class Person extends ManagedRecord {
  val name: String
  val schoolId: RecordId[School]
} 

case class Student (
  val name: String = null,
  val classYear: Int = -1,
  val schoolId: RecordId[School] = Schools.unsavedId,
  val id : RecordId[Student] = People.students.unsavedId
)
extends Person

case class Teacher (
  val name: String = null,
  val rating: Int = -1,
  val schoolId: RecordId[School] = Schools.unsavedId,
  val id : RecordId[Teacher] = People.teachers.unsavedId
)
extends Person

case class StrangePerson (
  val name: String = null,
  val personType: String = null,
  val schoolId: RecordId[School] = Schools.unsavedId,
  val id: RecordId[StrangePerson] = People.strange.unsavedId
)
extends Person

object People
  extends VariantRecordManager[Person]( PeopleDb( "people" ), "person_type")
{
  val students = new TaggedVariant[ Student ]("student")
  val teachers = new TaggedVariant[ Teacher ]("teacher")
  val strange  = new CatchAllVariant[ StrangePerson ]
}

// The actual spec

class VariantSpec
  extends Spec 
  with ShouldMatchers
  with BeforeAndAfterEach
  with RobolectricTests
  with SerializationTestHelpers
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
        List( "_id", "class_year", "name", "person_type", "rating", "school_id" ))
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

    it ("should report its discriminant") {
      People.students.discriminant should be ("student")
    }

    it ("should find pre-placed records") {
      val students = People.students.order("name").fetchOnThisThread
      students should have size (4)
      students.map{_.name} should equal (PeopleDb.studentNames)
      students.map{_.classYear} should equal (Seq(2011, 2014, 2011, 2014))
    }

    it ("should be able to insert") {
      People.students.onThisThread (Save (Student ("Pigpen", 2010)))
      People.students.count.fetchOnThisThread should equal (
        PeopleDb.numStudents + 1)

      val pigpens = People.students.whereEq("name"->"Pigpen").fetchOnThisThread
      pigpens should have size (1)

      val pigpen = pigpens(0)
      pigpen.name should equal ("Pigpen")
      pigpen.classYear should equal (2010)
    }

    it ("should be able to update (and find by ID)") {
      val lucy = 
        (People.students.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      People.students.onThisThread (Save (lucy.copy (classYear = 2010)))

      val nucy = People.students.findOnThisThread (lucy.id) 
      nucy.classYear should equal (2010)
    }

    it ("should be able to delete") {
      val lucy = 
        (People.students.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      People.students.onThisThread (Delete (lucy))

      val students = People.students.order("name").fetchOnThisThread
      students should have size (3)
      students.map{_.name} should equal (
        PeopleDb.studentNames.filterNot{_ == lucy.name})
    }
  }

  describe( "combined manager for all variants" ) {

    describe ("finding pre-placed records") {

      def people = People.order("name").fetchOnThisThread

      it ("should get number of records and common fields right") {
        people should have size (PeopleDb.numPeople)
        people.map{_.name} should equal (PeopleDb.peopleNames)
      }

      it ("should get specific fields right") {
        val teachers = 
          people.flatMap{_ match {case t:Teacher => Seq(t); case _ =>Seq.empty}}

        teachers(0).rating should equal (6)
      }

      it ("should retrieve records with unknown discriminants via catch-all") {
        val strange =
          people.flatMap{_ match { case s:StrangePerson => Seq(s); 
                                   case _ => Seq.empty }}

        strange should have size (1)
        strange(0).name should equal ("Snoopy")
        strange(0).personType should equal ("beagle")
      }
    }

    it ("should be able to insert") {
      People.onThisThread (Save (Student ("Pigpen", 2010)))
      People.count.fetchOnThisThread should equal (PeopleDb.numPeople + 1)

      val pigpens = People.whereEq("name"->"Pigpen").fetchOnThisThread
      pigpens should have size (1)

      val pigpen = pigpens(0)
      pigpen.name should equal ("Pigpen")
      pigpen.asInstanceOf[Student].classYear should equal (2010)
    }

    it ("should be able to update (and find by ID)") {
      val lucy = (People.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      People.onThisThread(Save(lucy.asInstanceOf[Student].copy(classYear=2010)))

      // XXX wart --- ideally shouldn't need the cast here.
      val nucy = People.findOnThisThread (lucy.id.asInstanceOf[RecordId[Person]]) 
      nucy.asInstanceOf[Student].classYear should equal (2010)
    }

    it ("should be able to delete") {
      val lucy = (People.whereEq("name"->"Lucy van Pelt").fetchOnThisThread)(0)
      People.onThisThread (Delete (lucy))

      val people = People.order("name").fetchOnThisThread
      people should have size (PeopleDb.numPeople - 1)
      people.map{_.name} should equal (
        PeopleDb.peopleNames.filterNot{_ == lucy.name})
    }
  }

  describe ("HasMany associations with variant targets") {

    def blagoSchool = {
      val blago = "Rod Blagojevich Junior Academy"
      (Schools.whereEq( "name" -> blago ).fetchOnThisThread)(0)
    }

    describe ("single variant target") {
      it ("should find all the records") {
        val blagoStudents = blagoSchool.students.order("name").fetchOnThisThread
        val blagoNames = 
          PeopleDb("people")
            .whereEq("school_id"   -> PeopleDb.blagoSchoolId,
                     "person_type" -> "student")
            .order("name")
            .select("name")
            .map{ _.getString(0) }
        blagoStudents.map{ _.name } should equal (blagoNames)
      }
    }

    describe ("combined target") {
      it ("should find all the records") {
        val blagoPeople = blagoSchool.people.order("name").fetchOnThisThread
        val blagoNames = 
          PeopleDb("people")
            .whereEq("school_id" -> PeopleDb.blagoSchoolId)
            .order("name")
            .select("name")
            .map{ _.getString(0) }
        blagoPeople.map{ _.name } should equal (blagoNames)
      }
    }
  }

  describe("serialization and deserialization of records and IDs") {
    it ("should be able to handle IDs") {
      val students = People.students.order("name").fetchOnThisThread
      val tuple = (students(0).id, students(1).id)
      assertSerializationPreservesEquality( tuple )
    }
    it ("should be able to handle whole records") {
      val students = People.students.order("name").fetchOnThisThread
      val tuple = (students(0), students(1))
      assertSerializationPreservesEquality( tuple )
    }
    it ("should be able to use a deserialized ID in a find") {
      val undoneItems = People.students.order("name").fetchOnThisThread
      val roundtripIdObj = serializationRoundTrip( undoneItems(0).id )
      val roundtripId = roundtripIdObj.asInstanceOf[ RecordId[ Student ]]
      val item = roundtripId.fetchOnThisThread
      item should equal (undoneItems(0))
    }
  }
}
