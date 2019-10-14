package tests

import org.eclipse.{lsp4j => l}
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.meta.internal.metals.MetalsServerConfig
import scala.meta.internal.metals.StatisticsConfig

object TypeDefinitionLspSuite
    extends BaseLspSuite("typeDefinition")
    with TestHovers {
  override def serverConfig: MetalsServerConfig =
    super.serverConfig.copy(
      statistics = new StatisticsConfig("diagnostics")
    )

  override def testAsync(
      name: String,
      maxDuration: Duration = Duration("3min")
  )(run: => Future[Unit]): Unit = {
    if (isWindows) {
      // src.zip is missing on Appveyor which breaks definition tests.
      ignore(name) {}
    } else {
      super.testAsync(name, maxDuration)(run)
    }
  }

  def check(name: String, testCase: String, expected: String): Unit = {
    val base = testCase.replaceAll("@@", "")
    testAsync(name) {
      cleanWorkspace()
      for {
        _ <- server.initialize(
          s"""/metals.json
             |{"a":{}}
             |/a/src/main/scala/a/Main.scala
             |$base
      """.stripMargin
        )
        _ <- server.didOpen("a/src/main/scala/a/Main.scala")
        _ <- server.assertTypeDefinition(
          "a/src/main/scala/a/Main.scala",
          testCase,
          s"|${workspace.toURI.toString}$expected|"
        )
      } yield ()
    }
  }

  def locationFromString(str: String): l.Location = {
    def positionFromString(str: String): l.Position = {
      val ints = str.split(":").map(_.toInt)
      new l.Position(ints.head, ints.last)
    }

    val split = str.split("""/\\""")
    assertEquals(split.length, 3, "Incorrect location string format")
    val (uri, rest) = (split.head, split.tail)
    val range =
      new l.Range(positionFromString(rest.head), positionFromString(rest.last))
    new l.Location(
      workspace.toURI.resolve(uri).toString.replaceFirst("/", "///"),
      range
    )
  }

  def createLocation(
      uri: String,
      begin: (Int, Int),
      end: (Int, Int)
  ): l.Location = {
    val startPos = new l.Position(begin._1, begin._2)
    val endPos = new l.Position(end._1, end._2)

    new l.Location(uri, new l.Range(startPos, endPos))
  }

  check(
    "int",
    """
      |object Main {
      |  val ts@@t: Int = 2
      |}""".stripMargin,
    ".metals/readonly/scala/Int.scala/\\25:21/\\25:24"
  )

  check(
    "basic",
    """
      |package a
      |
      |class Main(i: Int) {}
      |
      |object Main extends App {
      |  val te@@st = new Main(1)
      |}
        """.stripMargin,
    "a/src/main/scala/a/Main.scala/\\3:6/\\3:9"
  )

  check(
    "method",
    """
      |package a
      |
      |class Main(i: Int) {}
      |
      |object Main extends App {
      |  def foo(mn: Main): Unit = {
      |     println(m@@n)
      |  }
      |}
        """.stripMargin,
    "a/src/main/scala/a/Main.scala/\\3:6/\\3:9"
  )

  check(
    "method-definition",
    """
      |package a
      |class Main(i: Int) {}
      |
      |object Main extends App {
      |  def foo(mn: Main): Unit = {
      |     println(mn)
      |  }
      |
      |  fo@@o(new Main(1))
      |}
        """.stripMargin,
    "a/src/main/scala/a/Main.scala/\\5:6/\\5:8"
  )

}
