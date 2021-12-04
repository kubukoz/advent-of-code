import scala.io.Source

import scala.util.Using

object lib {

  def readAllLines(fileName: String): List[String] =
    Using(Source.fromResource(fileName))(_.getLines.toList).get

  def readAll(fileName: String): String = readAllLines(fileName).mkString("\n")

}
