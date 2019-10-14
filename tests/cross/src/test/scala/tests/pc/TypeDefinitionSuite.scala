package tests.pc

object TypeDefinitionSuite extends BaseTypeDefinitionSuite {

  check(
    "basic",
    """
      |class Main(i: Int) {
      |
      |}
      |
      |object Main {
      |  val ts@@t = new Main(2)
      |}""".stripMargin,
    "Main.scala/\\2:6/\\2:9"
  )

  check(
    "method",
    """
      |class Main(i: Int) {}
      |
      |object Main {
      | def tst(m: Main): Unit = {}
      |
      |  tst(new M@@ain(2))
      |}""".stripMargin,
    "Main.scala/\\2:6/\\2:9"
  )

  check(
    "method-def",
    """
      |object Main {
      |  def tst(): Unit = {}
      |
      |  ts@@t()
      |}""".stripMargin,
    "Main.scala/\\3:6/\\3:8"
  )

}
