package org.positronicnet.sample.todo_cp

import org.positronicnet.db.Database

import org.positronicnet.orm._
import org.positronicnet.content._

// Definition of a ContentProvider for TodoList data.

// Start by defining the DB schema for the underlying data repository.

object TodoDb extends Database( filename = "todos.sqlite3", logTag = "todo" ) 
{
  // This gets fed to a SQLiteOpenHelper, which implements the following
  // default behavior (unless overridden, of course):
  //
  // "version" is the length of schemaUpdates.
  // "onUpdate" runs all the updates from oldVersion to newVersion.
  // "onCreate" just runs 'em all.

  def schemaUpdates =
    List(""" create table todo_lists (
               _id integer primary key,
               name string
             )
         """,
         """ create table todo_items (
               _id integer primary key,
               todo_list_id integer,
               description string,
               is_done integer
             )
         """
        )
}

// Now define the ContentProvider object that exports that stuff, according
// to the conventions defined in our TodoContract Java class.  (It's in Java
// to follow platform conventions, although what's there would be cleaner
// as a Scala singleton.)

class TodoProvider extends PositronicContentProvider
{
  import TodoContract._      // make 'static' stuff visible w/o prefix

  def onCreate = { TodoDb.openInContext( getContext ); true }

  // Queries for todo lists...

  trait ListOps extends UriMatch {

    // When deleting a list, or several, we also want to delete the items...

    override def delete( req: ParsedRequest ) = {

      val qry = queryForParsedRequest( req )

      // Clearly need some cleaner machinery for faking up joins...

      val whereValues = 
        if (qry.whereValues == null) new Array[ContentValue](0) // yuck
        else qry.whereValues.map{ CvString(_) }                 // double yuck

      val whereString =
        if (qry.whereString == null) "2+2=4"
        else qry.whereString

      val itemCond = "todo_list_id in (" + 
                     "select _id from todo_lists where " + whereString + ")"
      TodoDb( "todo_items" ).where( itemCond, whereValues: _* ).delete

      super.delete( req )
    }
  }

  new UriMatch( TODO_LISTS_URI,                  Seq.empty, 
                dirContentType( TODO_LIST_TYPE ),
                TodoDb("todo_lists"))
    with ListOps

  new UriMatch( TODO_LISTS_PREFIX+"/=",          Seq("_id"),
                rowContentType( TODO_LIST_TYPE ),
                TodoDb("todo_lists"))
    with ListOps

  // Queries for items

  new UriMatch( TODO_LISTS_PREFIX+"/=/items",    Seq("todo_list_id"),
                dirContentType( TODO_ITEM_TYPE ),
                TodoDb("todo_items"))

  new UriMatch( TODO_LISTS_PREFIX+"/=/items/=",  Seq("todo_list_id", "_id"),
                rowContentType( TODO_ITEM_TYPE ),
                TodoDb("todo_items"))
}
