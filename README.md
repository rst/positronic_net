#Positronic Net

Positronic Net is an attempt to reduce the amount of boilerplate
coding required for Android programming to just connect framework
components together.  It's written in Scala, and uses Scala features
(traits, a/k/a mixins, and functional arguments) to let programmers
say what they mean without so much chatter about components of the
framework itself.

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

##Fluid notation for dealing with Sqlite

A sample, implementing a soft deletion scheme:

    val dbItemsAll = TodoDb("todo_items").whereEq("todo_list_id"->id)
    val dbItems    = dbItemsAll.whereEq( "is_deleted" -> false )

    // ... and later ...
    
    def deleteWhereDone = {
      dbItemsAll.whereEq( "is_deleted" -> true ).delete // purge the last batch
      dbItems.whereEq( "is_done" -> true ).update( "is_deleted" -> true )
    }

##"Change listener" framework

It's generally good practice on Android to manipulate databases, and
perform other background operations, on a thread different from the
one that runs the user interface.  But it can be a pain to arrange.
Positronic Net includes a `CursorSourceAdapter` class which can be
used like so:

    class TodosAdapter( activity: PositronicActivity )
     extends CursorSourceAdapter( activity, 
                                  source = TodoLists,
                                  converter = TodoList.fromCursor(_),
                                  itemViewResourceId = R.layout.todos_row )
    {
      def bindItem( view: View, list: TodoList ) =
        view.asInstanceOf[ TextView ].setText( list.name )
    }

You use this by instantiating it and calling `listView.setAdapter(...)`,
as with any other adapter.  The difference is that this one arranges
with the cursor source (actually a `ChangeNotifications[Cursor]`) to
hear about updates to the underlying data, within the lifetime of its
enclosing `activity`.  (It stops listening when the `activity` is
destroyed.)  When changes occur, the attached view gets updated, on
the `activity`'s UI thread.

This is an attempt to meet the same basic need as the `CursorLoader`
machinery in Android 3.0, but with better encapsulation and less
chatter.

##Miscellaneous shorthands

The library also provides alternate overloads for a lot of standard
framework APIs (usually to give them functional arguments instead of,
say, a `Runnable`), which allow common usage patterns to be more
clearly (and briefly) expressed.  I've also tried to cut down on
other forms of chatter --- if you *always* have to call `super.foo()`,
the Positronic Net alternate version of `foo` will often do that for
you.  (Again, if you're doing something unusual, the standard APIs
and entrypoints are always available.)