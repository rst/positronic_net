#Positronic Net

Positronic Net is an attempt to add some MVC flavor to the process of
developing Android apps, and to reduce the amount of boilerplate code
required to just connect framework components together.  It's written
in Scala, and uses Scala features (traits, a/k/a mixins, and "lambdas"
or functional arguments) to let programmers say what they mean without
so much chatter about components of the framework itself.

It builds using `sbt` version 0.7.7 and the [sbt-android-plugin](https://github.com/jberkel/android-plugin).

The project source includes the library itself (in `src`) and a simple
sample app (as an sbt subproject); reading the sample app code might
be the best way to get a feel for what the library is trying to
accomplish.

#Features

What's in the package right now includes the following:

##Event handler declarations in a style reminiscent of jQuery:

    listsView.onItemClick { (view, posn, id) => viewListAt( posn ) }

    findView( TR.addButton ).onClick { doAdd }
    findView( TR.newListName ).onKey( KeyEvent.KEYCODE_ENTER ){ doAdd }

    onOptionsItemSelected( R.id.undelete ) { doUndelete }

These come from a `PositronicHandler` trait and friends, which come
premixed into `PositronicActivity`, `PositronicButton`, and so forth.
(These extend the corresponding Android platform classes, so if you
don't have a Positronic shorthand for something, or just don't like
it, the standard API is fully available.)

##A fairly simple, lightweight ORM

Designing an object-relational mapper for Android means dealing with
some unusual constraints.  Most notably, it's generally good practice
on Android to manipulate databases, and perform other background
operations, on a thread different from the one that runs the user
interface.  That means that a typical ORM API along the lines
of, say
    
    myRecord.updateDescription( "... new stuff ..." )
    myRecord.save

is problematic.  If it does the obvious thing --- save the record,
and return to the caller --- and an Activity runs that code on its
"UI thread", then the user interface blocks while the save is in
progress.  That can be enough to create a laggy experience, if 
other file system activity is going on, and the code has to wait
for shared locks.  This problem gets even knottier when you
consider queries:

    val recs = TodoItems.whereEq( "isDone" -> false ).find

If an activity needs the records (say, to display them), and asks for
them this way, then it's potentially blocked while the query is going
on.  But if not, then how does activity code ever load the records'
values into its widgets?

Positronic Net's ORM tries to deal with this by borrowing some ideas
from Actor-based libraries:  We consider activities and the ORM to be
concurrent processes, which exchange data via messages.  The messages
can include ORM-managed records going either way, which are considered
to be immutable snapshots of the mutable persistent state that's managed
by the ORM --- or requested changes to that state.

###Basic ORM usage

To try to make that clear with a concrete example:  Imagine we have
TodoItems which have a task description, and a "done" state.

    case class TodoItem( todoListId: Long    = ManagedRecord.unsavedId,
                         description: String = null, 
                         isDone: Boolean     = false,
                         id: Long            = ManagedRecord.unsavedId 
                       )
      extends ManagedRecord( TodoItems )
    {
      def setDescription( s: String ) = copy( description = s )
      def setDone( b: Boolean )       = copy( isDone = b )
    }

    object TodoItems extends RecordManager[ TodoItem ]( TodoDb( "todo_items" ))

Here, `TodoItems` is declared as a singleton `RecordManager` which is
ultimately responsible for managing the persistent storage of things
of class `TodoItem`.  An activity could ask it to save one by saying,
say:

    TodoItems ! Save( oldItem.setDescription( "... new description ..." ))

This arranges for the I/O to happen on a background thread; the activity's
main thread can proceed immediately.  Queries are a bit more complicated.
Here's an example, which also shows a little other machinery:

    TodoItems.whereEq( "isDone" -> false ) ! Fetch { recs =>
      for (rec <- recs) ...
    }

Here, we're constructing a restricted entity representing a subset of
the TodoItems, and sending it the `Fetch` message, which has a body
that is invoked, with the records as an argument, when they are
available.  (As a special-case favor to the needs of the Android UI,
we use an `android.os.Handler` to run the body of a `Fetch` request,
and similar requests, on the thread that invoked them --- so if an
activity does a `Fetch` on its main UI thread, it's safe to manipulate
UI components within the `Fetch` body.)

###"Change notification" framework

It can still be bothersome to have to explicitly re-query the ORM
to refresh displays when relevant state changes.  If you have an
`Activity` which manages a list of some kind, you need to update it
when items are added or deleted, and perhaps when the state changes
otherwise.  It's more convenient if the UI components register with
the ORM when the list changes, so that a 

    TodoItems ! Delete( ... )

or

    TodoItems ! Save( ... )

makes them update automatically, without further code at the point
of the delete and update.

So, Positronic Net provides UI components that do that, for example:

    class TodoItemsAdapter( activity: PositronicActivity, 
                            query: Notifier[IndexedSeq[TodoItem]] )
      extends IndexedSeqSourceAdapter( activity,
                                       TodoItems,
                                       itemViewResourceId = R.layout.todo_row )
    {
      override def bindView( view: View, it: TodoItem ) =
        view.asInstanceOf[ TodoItemView ].setTodoItem( it )
    }

Whenever `TodoItems` (or one of its subsidiary scopes created by
`whereEq` and friends) gets updated, it will automatically pass an
updated sequence to this adapter, which will in turn update any
associated views; the activity code that caused the update doesn't
have to worry about it.

(Footnote:  an `activity` is passed in to the constructor so the
`IndexedSeqSourceAdapter` can automatically _stop_ watching the
`TodoItems` when the associated activity quits.)

If you're doing something idiosyncratic, the underlying machinery
is also available to be used directly, e.g. by `AddWatcher` and
`StopWatcher` requests.

###One-to-many associations

The ORM also currently has some support for many-to-one and
one-to-many associations.  An app which manages multiple to-do
lists might do something like this:  note the `HasMany` and
`BelongsTo` items:

    case class TodoList( name: String = null,
                         id: Long     = ManagedRecord.unsavedId )
      extends ManagedRecord( TodoLists )
    {
      def setName( s: String ) = copy( name = s )

      lazy val items = new HasMany( TodoItems )
      lazy val doneItems = items.whereEq( "is_done" -> true )
    }

If you have a `TodoList`, then `myList.items` will automatically
query for the set of `TodoItems` whose `todo_list_id` in the database
matches the `id` of the list itself.  There's also a `BelongsTo` class
which offers limited support for the one-to-many side of the association
(at least enough to be able to `Fetch` the list associated with an item!)

Full examples of this can be seen by viewing the todo-list sample app
in the `sample` directory; the above has been (slightly) simplified
excerpts.

##Miscellaneous shorthands

The library also provides alternate overloads for a lot of standard
framework APIs (usually to give them functional arguments instead of,
say, a `Runnable`), which allow common usage patterns to be more
clearly (and briefly) expressed.  I've also tried to cut down on
other forms of chatter --- if you *always* have to call `super.foo()`,
the Positronic Net alternate version of `foo` will often do that for
you.  (Again, if you're doing something unusual, the standard APIs
and entrypoints are always available.)