package tests.pc

import tests.BasePCSuite
import scala.concurrent.duration.Duration
import scala.meta.internal.metals.CompilerOffsetParams
import collection.JavaConverters._
import org.eclipse.{lsp4j => l}

class BaseTypeDefinitionSuite extends BasePCSuite {

  override def beforeAll(): Unit = {
    indexJDK()
    indexScalaLibrary()
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
    new l.Location(uri, range)
  }

  def locationToString(loc: l.Location): String = {
    val (uri, start, end) =
      (loc.getUri, loc.getRange.getStart, loc.getRange.getEnd)
    s"|$uri/\\${start.getLine}:${start.getCharacter}/\\${end.getLine}:${end.getCharacter}|"
  }

  def check(
      name: String,
      code: String,
      expected: String,
      duration: Duration = Duration("3 min")
  ): Unit = {
    val filename = {
      val tmp = code.lines
        .map(_.trim)
        .find(s => s.startsWith("class") || s.startsWith("object"))
        .get
        .replaceFirst("[(]", " ")
      val start = tmp.indexWhere(_.isUpper)
      val end = tmp.indexOf(" ", start)

      s"${tmp.substring(start, end)}.scala"
    }

    val raw = "package a\n" + code
    val paramsT = params(raw, filename)
    test(name) {
      pc.typeDefinition(
          CompilerOffsetParams(
            filename,
            paramsT._1,
            paramsT._2
          )
        )
        .get
        .asScala
        .toList match {
        case h :: _ =>
          val loc = locationToString(h)
            .dropWhile(_ == '|')
            .takeWhile(_ != '|')
            .stripPrefix("file://")
          val exp = expected
            .dropWhile(_ == '|')
            .takeWhile(_ != '|')
            .stripPrefix("file://")
          assertEquals(loc, exp)
        case _ =>
          fail("typeDefinition didn't produce any results")
      }
    }
  }

}
