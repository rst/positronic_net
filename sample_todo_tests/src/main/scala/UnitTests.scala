package org.positronicnet.todo.tests

import junit.framework.Assert._
import _root_.android.test.AndroidTestCase

class UnitTests extends AndroidTestCase {
  def testPackageIsCorrect {
    assertEquals("org.positronicnet.sample.todo", getContext.getPackageName)
  }
}
