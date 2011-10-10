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

