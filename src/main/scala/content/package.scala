package org.positronicnet

/** The `org.positronicnet.content` package provides common infrastructure,
  * and a common fluid interface, for
  * accessing both SQLite Databases (via the [[org.positronicnet.db.Database]]
  * class) and Android `ContentProvider`s (via
  * [[org.positronicnet.content.PositronicContentResolver]]).  This interface
  * is also used by the [[org.positronicnet.orm]] package, which provides
  * a common higher-level structure for accessing the same resources, where
  * it proves convenient.
  *
  * The user-visible facilities of the infrastructure consist largely of
  * the fluid interface for queries.  A brief example:  Suppose you have
  * a [[org.positronicnet.db.Database]] declared as follows:
  * {{{
  *     class TodoDatabase( filename: String )
  *       extends Database( filename = filename, logTag = "todo" ) 
  *     {
  *       def schemaUpdates =
  *         List(""" create table todo_items (
  *                    _id int identity,
  *                    todo_list_name varchar(100),
  *                    description varchar(100),
  *                    is_done integer
  *                  )
  *              """)
  *     }
  * }}}
  * Then `TodoDatabase("todo_items")` will yield a
  * [[org.positronicnet.content.ContentQuery]] object which can be used to
  * manipulate the rows of the `todo_items` table in the following ways:
  * {{{
  *     // get query for "all the items"
  *
  *     val qry = TodoDatabase("todo_items")
  *
  *     // From that, get a query for "all items on one list"
  *
  *     val bigListItems = qry.whereEq( "todo_list_name" -> "big list" )
  *
  *     // Do a few things with that.  Note that on insert, the
  *     // column values are *not* defaulted from the conditions.
  *
  *     val newId = bigListItems.insert( "todo_list_name" -> "big list",
  *                                      "description" -> "write docs",
  *                                      "is_done" -> false )
  *     bigListItems.where( "_id < ?", 50 ).update( "is_done" -> true )
  *     bigListItems.whereEq( "is_done" -> true ).delete
  * }}}
  * [[org.positronicnet.content.PositronicContentResolver]] can be used
  * to deal with Android ContentProviders in a similar style.
  */

package object content                  // empty; hook for Scaladoc

