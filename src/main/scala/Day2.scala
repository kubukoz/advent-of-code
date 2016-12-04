object Day2 {
  def wrappingPaper(input: Array[String]) =
    input.map(toIntArray)
      .map(l => l.indices.combinations(2).map(_.map(l).product).toList)
      .map(areas => 2 * areas.sum + areas.min).sum

  def ribbon(input: Array[String]) =
    input.map(toIntArray)
      .map(list => list.sorted.take(2).sum * 2 + list.product).sum

  private def toIntArray(s: String) = s.split("x").map(_.toInt)


  def main(args: Array[String]) {
    val source = io.Source.fromURL(getClass.getResource("/day2.txt"))
    val input = source.getLines().toArray

    source.close()

    println(wrappingPaper(input))
    println(ribbon(input))
  }
}
