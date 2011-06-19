package rst.todo

import _root_.android.app.Activity
import _root_.android.os.Bundle
import _root_.android.widget.TextView

class TodoActivity extends Activity {
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(new TextView(this) {
      setText("hello, world")
    })
  }
}
