package org.positronicnet

/** The [[org.positronicnet.notifications]] package provides machinery to
  * help "business model" classes (the M in MVC) communicate with UI code
  * that wants to View or Control their state.  In particular, it allows
  * UI components to register for updates on model state that they're
  * interested in.
  *
  * It also provides primitives for Actor-style concurrency management,
  * so Android UI code can request a change, and some time later be notified
  * of the result (via a watcher or callback), without the Activity's main
  * UI thread being blocked waiting in the meantime.
  *
  * The base [[org.positronicnet.notifications.Notifier]] trait specifies a set of actions for querying
  * the state, and requesting updates when it changes.  Subclasses
  * (e.g., [[org.positronicnet.orm.RecordManager]]) typically support
  * other actions which request changes to happen (which may, in turn,
  * cause watchers to be notified).
  *
  * The primary reason for doing things in an actor-style syntax in
  * Android is to deal with the fairly stringent best-practice requirements
  * for Android "user interface threads" --- specifically, the requirement
  * that they shouldn't be waiting for anything else, even local disk I/O
  * (let alone, say, a network operation).  So, all that stuff needs to
  * happen on background threads; we use actor style to help keep the
  * concurrency manageable.
  *
  * However, there are also circumstances where concurrency is a nuisance.
  * For instance, an Android `Service` or `BroadcastReceiver` may want to
  * be sure that an operation has completed before signalling that it has
  * finished itself.
  *
  * For these situations, you can also request that an action be performed
  * `onThisThread`, as in
  *
  *   `notifier.onThisThread( Fetch{ value => ... } )`
  *
  * which performs the `Fetch` and runs the body on the current thread,
  * forgoing all concurrency.  (Though for this particular case, there's
  * a special-case `fetchOnThisThread` method which simply fetches the
  * monitored state and returns it as the value.)
  *
  * As the name suggests, the notification machinery itself mainly
  * handles the business of notifying other code (typically an `Activity`
  * or `Service`) of current state, and providing updates.
  *
  * Accordingly, these classes aren't often used directly; instead,
  * you declare a subclass which actually provides something to monitor
  * --- or use other facilities that do that for you.  If the state is
  * held in a [[org.positronicnet.db.Database]], for instance, you might
  * want to use the [[org.positronicnet.orm]].
  */

package object notifications            // empty; hook for Scaladoc

