import scala.language.postfixOps

object Day17 {
  def main(args: Array[String]) {
    val input =
      """33
        |14
        |18
        |20
        |45
        |35
        |16
        |35
        |1
        |13
        |18
        |13
        |50
        |44
        |48
        |6
        |24
        |41
        |30
        |42""".stripMargin.split("\n").map(_.toInt)
    val target = 150

    val allCombsBecauseImLazy = (2 to input.length).flatMap(input.indices.combinations(_).map(_.map(input.apply)))
    val validCombs = allCombsBecauseImLazy.filter(_.sum == target)
    println("Part 1: " + validCombs.size)
    println("Part 2: " + validCombs.count(_.size == validCombs.map(_.size).min))
  }
}
