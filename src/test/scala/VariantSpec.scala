package org.positronicnet.orm.variantTest

import org.positronicnet.orm._
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
  override def beforeEach = PeopleDb.openInContext( Robolectric.application )
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
}
