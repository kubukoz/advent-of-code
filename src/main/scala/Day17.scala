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

    //    val input = Array(20, 15, 10, 5, 5)

    def trueCombinations(of: Array[Int], length: Int): List[List[Int]] = {
      of.indices.combinations(length).map(_.map(of.apply).toList).toList
    }
    val target = 150
    val allCombsBecauseImLazy = (2 to input.length).flatMap(trueCombinations(input, _))
    val validCombs = allCombsBecauseImLazy.filter(_.sum == target)
    println(s"Part 1: ${validCombs.size}")
    println("Part 2: " + validCombs.count(_.size == validCombs.map(_.size).min))
  }
}
