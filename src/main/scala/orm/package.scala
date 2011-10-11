package org.positronicnet

/** The Positronic Net ORM tries to abstract away a lot of the boilerplate
  * involved in dealing with data managed through cursors (or, more
  * precisely, any data which can be accessed through an
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
  * [[org.positronicnet.notifications]] action sending, q.v.)
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
  * It's even possible to add watchers on a [[org.positronicnet.orm.Scope]]
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
  */

package object orm                      // empty --- hook for Scaladoc
