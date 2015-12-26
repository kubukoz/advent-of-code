import scala.collection.mutable

object Day20 {
  def main(args: Array[String]) {
    val input = 36000000

    val uses = mutable.Map.empty[Int, Int]

    val houses = Stream.iterate(800000)(_+1)

    houses.find { i =>
      val sum = (1 to i).filter { elf =>
        if (uses.getOrElseUpdate(elf, 0) < 50) {
          uses(elf) += 1
          i % elf == 0
        } else false
      }.map(_ * 11).sum
      sum >= input
    } foreach println
  }
}