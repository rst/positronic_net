import sbt._

trait Defaults {
  def androidPlatformName = "android-7"
}
class Parent(info: ProjectInfo) extends ParentProject(info) {
  override def shouldCheckOutputDirectories = false
  override def updateAction = task { None }

  lazy val positronic_net = project(".", "PositronicNet", new LibProject(_))
  lazy val todo = project("sample_todo_app", "SampleTodo", new SampleProject(_), positronic_net)
  lazy val todo_tests = project("sample_todo_tests",  "SampleTodoTests", new TestProject(_), positronic_net, todo)

  class LibProject(info: ProjectInfo) extends AndroidProject(info) with Defaults

  class SampleProject(info: ProjectInfo) extends AndroidProject(info) with Defaults with MarketPublish with TypedResources {
    val keyalias  = "change-me"
    val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"
  }

  class TestProject(info: ProjectInfo) extends AndroidTestProject(info) with Defaults
}
