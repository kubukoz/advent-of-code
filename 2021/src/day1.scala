object Day1 extends App {

  val input = lib.readAllLines("day1.txt")
  val values = input.filterNot(_.trim.isEmpty).map(_.toInt)

  def result(data: List[Int]) = data.zip(data.tail).count { case (before, after) => after > before }

  println("Part 1: " + result(values))
  println("Part 2: " + result(values.sliding(3).map(_.sum).toList))
}
