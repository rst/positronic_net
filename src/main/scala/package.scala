// Miscellaneous package objects, for the sake of Scaladoc.

package org {
  /** The Positronic Net framework tries to make Android programming in Scala
    * more pleasant, by providing shorthand notations for common tasks, in
    * the [[org.positronicnet.ui]] and elsewhere (without
    * hiding the platform API if you prefer it, or want to do something for
    * which there's no shorthand), and providing cleaner abstractions for
    * dealing with persistent storage (via the [[org.positronicnet.content]]
    * abstractions, and the [[org.positronicnet.orm]]).
    *
    * See below for details.
    */

  package object positronicnet         // Dummy for Scaladoc

  package positronicnet {
    /** Utilities for resource management */
    package object facility             // Dummy for Scaladoc

    /** Shorthand notations for common UI tasks */
    package object ui
  }
}

