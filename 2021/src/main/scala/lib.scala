import scala.io.Source

import scala.util.Using

object lib {

  def readAllLines(fileName: String): List[String] =
    Using(Source.fromResource(fileName))(_.getLines.toList).get

  def readAll(fileName: String): String = readAllLines(fileName).mkString("\n")

  def assertEquals[A](actual: A, expected: A, description: String = ""): Unit =
    if (actual != expected)
      Console
        .err
        .println(
          s"Assertion failed: expected $expected, got $actual" + Some(description)
            .filterNot(_.isEmpty())
            .mkString(" (", "", ")")
        )

}
