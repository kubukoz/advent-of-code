package com.kubukoz

package object adventofcode2016 {
  def fileLines(fileName: String): List[String] = {
    val source = io.Source.fromURL(getClass.getResource(fileName))
    val result = source.getLines().toList
    source.close()
    result
  }
}
