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
  * record]] pattern --- in lowercase.
  *
  * The Ruby on Rails ActiveRecord ORM was an important
  * influence, particularly in the framework's embrace of "convention
  * over configuration".  That's perhaps best explained by example:  If
  * you follow common naming conventions in for columns in tables and
  * for fields in your record classes, the framework will figure out
  * the mapping between them without needing explicit declarations.
  * Similarly for the names of foreign keys in associations, and so forth.
  * 
  * (If you have a reason for violating the conventions, you ''can''
  * declare that you're doing something else instead; see "explicit
  * field mapping" below for cases where it's reasonable to do that,
  * and an example of how it's done.  But that's only necessary when
  * you're doing something unusual.  And if you are, that becomes more
  * obvious to the next guy reading the code if the unconventional
  * code isn't buried among dozens of lines of conventional
  * boilerplate.)
  *
  * However, this is not a clone of the
  * Rails ActiveRecord ORM.  There are significant differences
  * --- most notably in the use of the Actor-like
  * [[org.positronicnet.notifications]] machinery to make it easy to
  * write client code that never blocks waiting for a query result or
  * an update.
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
  * [[org.positronicnet.orm.ManagedRecord]] class.  The conventional approach is
  * for the [[org.positronicnet.orm.ManagedRecord]] class to have a niladic
  * (no arguments) constructor --- or, at the very least, a single constructor
  * for which all arguments have defaults.  (If, for some reason, this doesn't
  * work for you, you can override `newRecord` in the
  * [[org.positronicnet.orm.RecordManager]] to do something else instead.
  * But the common pattern just works.)
  *
  * Accordingly, a minimal record declaration might be something like the
  * following:
  * {{{
  *     case class TodoItem( description: String    = null, 
  *                          isDone: Boolean        = false,
  *                          id: RecordId[TodoItem] = TodoItems.unsavedId 
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
  *    field named `id`, which will be mapped to the `_id`
  *    within the database (to match Android conventions).  The type of
  *    that field is declared as `RecordId[...]`.  For saved records, this
  *    just wraps the underlying `Long` in the obvious way; for unsaved
  *    records, there is special handling.  Also, a `RecordId` for a saved
  *    record can be used to retrieve the record itself, as discussed below.
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
  *
  * ==One-to-many and many-to-one associations==
  *
  * The Positronic Net ORM has some support for one-to-many and many-to-one
  * associations.  (Many-to-many associations would require bypassing a lot
  * of the usual Android helpers, and isn't likely to be possible for
  * `ContentProvider`s, where the API simply doesn't support the required
  * joins.)
  *
  * The available features are perhaps best explained by example.  Let's
  * say that we have a [[org.positronicnet.db.Database]] with the following
  * schema:
  * {{{
  *     def schemaUpdates =
  *       List(""" create table todo_lists (
  *                  _id integer primary key,
  *                  name string
  *                )
  *            """,
  *            """ create table todo_items (
  *                  _id integer primary key,
  *                  todo_list_id integer,
  *                  description string,
  *                  is_done integer
  *                )
  *            """)
  * }}}
  * The intent obviously is that we have multiple `todo_lists`, each of
  * which has its own set of `todo_items` --- those being the items whose
  * `todo_list_id` column matches the `id` of the corresponding row in
  * `todo_lists`.  (Some Rails influence may be perceptible here in the
  * conventions regarding plurals and so forth.)
  *
  * We'd like to be able to access the items given the list, and vice
  * versa.  Here's an example of how that can get mapped:
  * {{{
  *     case class TodoList( name: String = null,
  *                          id: RecordId[TodoList] = TodoLists.unsavedId )
  *       extends ManagedRecord( TodoLists )
  *     {
  *       lazy val items = new HasMany( TodoItems )
  *     } 
  *     
  *     object TodoLists extends RecordManager[ TodoList ](TodoDb("todo_lists"))
  *     
  *     case class TodoItem( todoListId: RecordId[TodoList]=TodoLists.unsavedId,
  *                          description: String = null, 
  *                          id: RecordId[TodoItem] = TodoItems.unsavedId )
  *       extends ManagedRecord( TodoItems )
  * }}}
  *
  * Here, [[org.positronicnet.orm.ManagedRecord.HasMany]] is a nested class
  * provided by the [[orm.positronicnet.orm.ManagedRecord]] superclass.
  * The `lazy val`s here is, as usual, intended to delay construction
  * of these objects until someone refers to them.  Constructing them
  * doesn't immediately cause any database I/O, but it still takes time
  * and storage space, and if no one's going to refer to them at all,
  * making them `lazy` avoids that overhead completely.
  *
  * So, what the heck are these things?
  *
  * The [[org.positronicnet.orm.ManagedRecord.HasMany]] is the more familiar
  * of the two --- it's a [[org.positronicnet.orm.Scope]], such as we might
  * get by saying
  * {{{
  *     case class TodoList( ... ) extends ManagedRecord( TodoLists )
  *     {
  *       lazy val items = TodoItems.whereEq( "todoListId", this.id )
  *     } 
  * }}}
  * and may be watched queried as such; for instance, code like this:
  * {{{
  *     myTodoList.items ! Fetch{ items => ... }
  *     myTodoList.items ! AddWatcher( key ) { items => ... }
  * }}}
  * works either way.  Being a [[org.positronicnet.orm.ManagedRecord.HasMany]],
  * though, it has two extra tricks.  First, it has a `create` method, which
  * returns a new `TodoItem` with the `todoListId` prepopulated.  Thus, for
  * example:
  * {{{
  *     val item = myTodoList.items.create
  *     val itemWithDescription = item.copy( description = "Wash Dog" )
  *     myTodoList.items ! Save( itemWithDescription )
  * }}}
  * (Note that the save gets sent to `mytodoList.items`, so that its watchers
  * --- or the watchers of any other `TodoItem` sub-scope with the exact same
  * conditions --- will be notified of the change.)
  *
  * Second, when a `TodoList` is deleted, the
  * [[org.positronicnet.orm.RecordManager]]s use some reflection to track
  * down `TodoItem`s associated with the vanishing lists, and to delete them
  * as well.
  *
  * However, it's also sometimes useful to go the other way --- to be able
  * to use a `TodoItem` to fetch the associated `TodoList`.  That can be done
  * by using the `RecordId` as a query source for the item in question, like so:
  * {{{
  *     myItem.todoListId ! Fetch { list =>
  *       Log.d( "XXX", "The list's name is: " + list.name )
  *     }
  * }}}
  */ 
package object orm                      // empty --- hook for Scaladoc
