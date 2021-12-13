package aoc

import scala.io.Source
import scala.concurrent.duration._

import scala.util.Using

object lib {

  def readAllLines(fileName: String): List[String] =
    Using(Source.fromResource(fileName))(_.getLines.toList).get

  def readAll(fileName: String): String = readAllLines(fileName).mkString("\n")

  def assertEquals[A](actual: => A, expected: A, description: String): Unit = {
    val start = System.nanoTime()
    val result = actual
    val end = System.nanoTime()
    val td = (end - start).nanos.toMillis

    if (result != expected)
      Console
        .err
        .println(
          s"Assertion failed: expected $expected, got $result" + Some(description)
            .filterNot(_.isEmpty())
            .mkString(" (", "", ")") + s" (${td}ms)"
        )
    else
      println(
        s"${Console.GREEN}Assertion passed ($description): $expected == $result (${td}ms)${Console.RESET}"
      )
  }

}
