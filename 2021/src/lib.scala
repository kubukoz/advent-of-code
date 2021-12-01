import java.nio.file.Files

import java.nio.file.Paths
import scala.io.Source
import scala.util.Using

object lib {

  def readAllLines(fileName: String): List[String] =
    Using(Source.fromFile(s"./resources/$fileName"))(_.getLines.toList).get

  def readAll(fileName: String): String = readAllLines(fileName).mkString("\n")

}
