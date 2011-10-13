package org.positronicnet

/** ==Basic Concepts==
  *
  * The Positronic Net ORM tries to abstract away a lot of the boilerplate
  * involved in dealing with data managed through cursors (or, more
  * precisely, any data which can be accessed through a
  * [[org.positronicnet.content.ContentRepository]]; that currently
  * includes both SQLite [[org.positronicnet.db.Database]]s and
  * Android `ContentProvider`s, via
  * [[org.positronicnet.content.PositronicContentResolver]] --- though
  * ContentProvider support is currently less than perfect, due to lack
  * of support for batched updates.
  *
  * It works by transparently mapping rows from the cursors to (and
  * from) simple Scala objects, following the
  * [[http://en.wikipedia.org/wiki/Active_record_pattern active
  * record]] pattern.  (That's lowercase; while the Ruby on Rails
  * ActiveRecord ORM was an important influence, as can be seen in
  * [[org.positronicnet.db.Database]] schema management, there are
  * differences --- most notably in the use of the Actor-like
  * [[org.positronicnet.notifications]] machinery to make it easy to
  * write client code that never blocks waiting for a query result or
  * an update.)
  *
  * So, for instance, if we have a [[org.positronicnet.orm.RecordManager]]
  * named `TodoItems`, for a [[org.positronicnet.orm.ManagedRecord]] class
  * named `TodoItem`, then
  * {{{
  *     TodoItems ! Fetch{ allTheItems => doSomethingWith( allTheItems ) }
  * }}}
  * will fetch all the items (on a background thread), and `doSomethingWith`
  * them when they're available,
  * {{{
  *     TodoItems ! Save( new TodoItem( "read the tutorial for more" ))
  * }}}
  * will save a new one,
  * {{{
  *     TodoItems ! AddWatcher( this ){ allTheItems =>
  *        handleUpdate( allTheItems ) }
  * }}}
  * will arrange for `handleUpdate` to be called whenever any of them change,
  * and so forth.
  *
  * (As with other uses of the [[org.positronicnet.notifications]] machinery,
  * the concurrent
  * {{{
  *     manager ! action
  * }}}
  * form of message sending expects to be called from an Android
  * `HandlerThread`, such as the main UI thread of an `Activity`, and
  * invocations of the `doSomethingWith` and `handleUpdate` callbacks
  * are posted back to that `HandlerThread` for execution --- mainly
  * to make it safe to manipulate an `Activity`'s `View`s in the
  * callbacks, which can only be safely done from the main UI thread.
  * Where that's inconvenient, as in a `BroadcastReceiver`, it's
  * better to use the `onThisThread` form of
  * [[org.positronicnet.notifications]] action sending, q.v.
  *
  * Often, you don't want to operate on all of the records in a table
  * at once.  So, starting with a
  * [[org.positronicnet.orm.RecordManager]], you can produce other
  * Actor-like objects called [[org.positronicnet.orm.Scope]]s (by
  * analogy to the `named_scope`s in the Rails ActiveRecord orm) which
  * refer to a restricted subset of the records, like so:
  * {{{
  *     var undoneItems = TodoItems.whereEq( 'isDone', false )
  *     undoneItems ! Fetch{ ... }
  * }}}
  *
  * It's even possible to add watchers on a [[org.positronicnet.orm.Scope]].
  * But there's a limit to the magic:
  * {{{
  *     undoneItems ! AddWatcher( tag ){ undoneUpdate(_) }
  *     undoneItems ! Save( someTodoItem )
  * }}}
  * will work, and will fire off `undoneUpdate` when the `Save` happens,
  * with the new values of `undoneItems`.  We also take care that if some
  * other code constructs a different [[org.positronicnet.orm.Scope]] with
  * the same conditions, updates on one will trigger watchers on the other.
  * (And of course, both will trigger watchers on `TodoItems` itself.)
  * However, if someone does a
  * {{{
  *     TodoItems ! Save( someRecord )
  * }}}
  * that ''won't'' trigger watchers on `undoneItems` --- even if `someRecord`
  * has `isDone` set to false.
  *
  * (Incidentally, it is possible to use `whereEq`, or the somewhat
  * clumsier but more general `where`, on a [[org.positronicnet.orm.Scope]]
  * to produce a further restricted [[org.positronicnet.orm.Scope]], as if
  * it was a [[org.positronicnet.orm.RecordManager]].  Or rather, the reverse;
  * [[org.positronicnet.orm.RecordManager]] actually extends
  * [[org.positronicnet.orm.Scope]].)
  *
  * The use of actor-like machinery suggests other Actor-like conventions
  * as well --- in particular, that the objects that are shared between
  * actors (in our case, say, between an [[org.positronicnet.orm.RecordManager]]
  * and Android `Activity` which displays its
  * [[org.positronicnet.orm.ManagedRecord]]s) should be immutable.
  *
  * As we've seen, the most important of those objects are the
  * [[org.positronicnet.orm.ManagedRecord]]s themselves, so the
  * usual convention is for them to be immutable.  It may be helpful to
  * think of them as ephemeral, immutable snapshots of the mutable state
  * which is, in effect, controlled by the
  * [[org.positronicnet.orm.RecordManager]].
  *
  * ==Declaring Record types, and associating them with data sources==
  *
  * So, to recap the above, we want to have immutable records shuffled
  * between client code and a [[org.positronicnet.content.ContentRepository]].
  * How are we going to set this up?
  *
  * Let's start with the records themselves.  The simplest way
  * to declare an immutable record with a bunch of fields in Scala is as a
  * `case class`.  We'd like to allow for that, adding as little extra
  * complexity as possible --- but we still need to add a bit for the
  * [[org.positronicnet.orm.RecordManager]] to be able to do its job.
  *
  * At a minimum, there needs to be some association between the record
  * class (a subclass of [[org.positronicnet.orm.ManagedRecord]]) and the
  * [[org.positronicnet.orm.RecordManager]]; we arrange for this by having
  * the [[org.positronicnet.orm.RecordManager]] be an argument to the
  * [[org.positronicnet.orm.ManagedRecord]] base class constructor.
  *
  * Furthermore, the [[org.positronicnet.orm.RecordManager]] has to be
  * able to instantiate new instances of its
  * [[org.positronicnet.orm.ManagedRecord]] class.  So, the
  * [[org.positronicnet.orm.ManagedRecord]] class needs to have a niladic
  * (no arguments) constructor --- or, at the very least, a constructor in
  * which all arguments have defaults.
  *
  * Accordingly, a minimal record declaration might be something like the
  * following:
  * {{{
  *     case class TodoItem( description: String = null, 
  *                          isDone: Boolean     = false,
  *                          id: Long            = ManagedRecord.unsavedId 
  *                        )
  *       extends ManagedRecord( TodoItem )
  *
  *     object TodoItems extends RecordManager[TodoItem]( TodoDb("todo_items"))
  * }}}
  * for the `todo_items` table of a [[org.positronicnet.db.Database]]
  * declared like so:
  * {{{
  *     object TodoDb extends Database( filename = "todos.sqlite3" ) 
  *     {
  *       def schemaUpdates =
  *         List(""" create table todo_items (
  *                    _id integer primary key,
  *                    description string,
  *                    is_done integer
  *                  )
  *              """
  *            )
  *     }
  * }}}
  *
  * Some points to note in this example:
  *
  *  - The [[org.positronicnet.orm.RecordManager]] doesn't have to be the
  *    companion object of the [[org.positronicnet.orm.ManagedRecord]] class
  *    that it manages (although it certainly can be).
  *  - The [[org.positronicnet.orm.ManagedRecord]] class needs to have a
  *    `Long`-valued field named `id`, which will be mapped to the `_id`
  *    within the database (to match Android conventions).
  *  - The [[org.positronicnet.orm.RecordManager]] will happily persist
  *    `Boolean` fields into `integer` columns by setting them to 0 or 1,
  *    following SQLite's recommended convention for booleans.  It will
  *    also persist numeric values (integers and floats) and strings into
  *    like-typed columns.  For more complicated datatypes, see below.
  *  - Scala field names are conventionally camelCased.  However, database
  *    column names are case-insensitive, and words between them are separated
  *    by underscores.  The [[org.positronicnet.orm.RecordManager]] 
  *    ordinarily converts between these conventions.  If you want to override
  *    this for some reason, again, see below.
  *  - The mapped fields don't really ''have'' to be constructor arguments
  *    of a `case class`; it isn't even strictly required that they be
  *    immutable.  However, one advantage of using `case class`es is that their
  *    automatically generated `copy` method makes it easy to generate dinked
  *    copies in which only one field has been changed; if you do things
  *    differently, you'll have to make other arrangements.  And if you
  *    make anything about the objects mutable, of course, you'll need to
  *    deal with the possibility of concurrent updates as well.
  *  - Aside from what's outlined above, there are no other methods that the
  *    [[org.positronicnet.orm.ManagedRecord]] is ''required'' to implement.
  *    However, providing a few "mutated-copy" methods is often a good idea,
  *    as a guide to intended usage; see the tutorial and sample app for
  *    examples.
  *
  * ==Explicit field mapping, and persisting unusual datatypes==
  *
  * As indicated above, a [[org.positronicnet.orm.RecordManager]] will
  * ordinarily map `likeThis` to columns `like_this`.  If, for some reason,
  * you want to override this convention, you can do it by having the
  * [[org.positronicnet.orm.RecordManager]] constructor call `mapField`
  * before anything else happens.
  *
  * One reason you might want to do this is to persist data of some
  * kind where there's no direct conversion.  Let's say, for instance,
  * that you want it to look like one of your
  * [[org.positronicnet.orm.ManagedRecord]] classes has a persistent
  * `java.util.Date`-valued field, called, say, `dueDate`.  However,
  * SQLite has no date-valued columns, so it won't work to shove them
  * in directly.
  *
  * One approach that works is to have the persistent column be a
  * 64-bit SQLite integer which gets mapped to a Scala `Long` called
  * `rawDueDate`, and then arrange the conversions:
  * {{{
  *     object MyDb extends Database( filename = "db.sqlite3" ) 
  *     {
  *       def schemaUpdates =
  *         List(""" create table projects (
  *                    _id integer primary key,
  *                    due_date integer,
  *                    ...
  *                  )
  *              """
  *            )
  *     }
  *
  *     case class Project( rawDueDate: Long = ...,
  *                         ... )
  *       extends ManagedRecord( Projects )
  *     {
  *       lazy val dueDate = new Date( rawDueDate )
  *       def changeDueDate( newDate: Date ) =
  *         this.copy( rawDueDate = newDate.getTime )
  *       ...
  *     }
  *
  *     object Projects extends RecordManager[ Project ](MyDb("projects")) {
  *       mapField( "rawDueDate", "due_date" )
  *     }
  * }}}
  *
  * If, for some reason, you want to do all the field-mapping explicitly,
  * and don't want the standard conventions to apply at all, use a
  * [[org.positronicnet.orm.BaseRecordManager]].  This is a base class of
  * [[org.positronicnet.orm.RecordManager]] which doesn't try to figure out
  * anything on its own, and hence, can't make mistakes; it may make sense to
  * use it when the underlying [[org.positronicnet.content.ContentRepository]]
  * is an Android `ContentProvider`, and the actual column names aren't
  * under your control to begin with.
  */

package object orm                      // empty --- hook for Scaladoc
