object Day3 extends App {

//   val input = """00100
// 11110
// 10110
// 10111
// 10101
// 01111
// 00111
// 11100
// 10000
// 11001
// 00010
// 01010""".split("\n").toList.map(_.toList)

  val input = lib.readAllLines("day3.txt").map(_.toList)

  val data = input
  val dataRoot = data

  val mostCommons = // Integer.parseInt(
    data
      .transpose
      .map(
        _.groupBy(identity).maxBy(_._2.size)._1
      )
      .mkString

  def mostCommonCount(i: Int) = data.map(_(i)).count(_ == mostCommons(i))
  // val leastC = data.count(_(i) == mostCommons(i))
  // 2 //
  // )//

  val leastCommons = // Integer.parseInt(
    data
      .transpose
      .map(
        _.groupBy(identity).minBy(_._2.size)._1
      )
      .mkString // ,
  //   2,
  // )

  // println(result * result2)

  def go(i: Int, data: List[List[Char]]): List[Char] = {

    val mostCommons = // Integer.parseInt(
      data
        .transpose
        .map(
          _.groupBy(identity).maxBy(_._2.size)._1
        )
        .mkString

    def mostCommonCount(i: Int) = data.map(_(i)).count(_ == mostCommons(i))

    val considered: Char =
      if (mostCommonCount(i).toDouble == data.size / 2.0)
        '1'
      else
        mostCommons(i)

    val filtered = data.filter { record =>
      record(i) == considered
    }

    if (filtered.size == 1)
      filtered.head
    else if (filtered.isEmpty)
      ???
    else {
      go(i + 1, filtered)
    }
  }

  def go2(i: Int, data: List[List[Char]]): List[Char] = {

    val leastCommons = // Integer.parseInt(
      data
        .transpose
        .map(
          _.groupBy(identity).minBy(_._2.size)._1
        )
        .mkString

    def leastCommonCount(i: Int) = data.map(_(i)).count(_ == leastCommons(i))

    val considered: Char =
      if (leastCommonCount(i).toDouble == data.size / 2.0)
        '0'
      else
        leastCommons(i)

    val filtered = data.filter { record =>
      record(i) == considered
    }

    if (filtered.size == 1)
      filtered.head
    else if (filtered.isEmpty)
      ???
    else {
      go2(i + 1, filtered)
    }
  }

  val r1 = go(0, data)
  val r2 = go2(0, data)

  // println(r1)
  // println(r2)
  // println(Integer.parseInt(r1.mkString, 2) * Integer.parseInt(r2.mkString, 2))
  // println(r1.mkString)
  println(Integer.parseInt(r1.mkString, 2) * Integer.parseInt(r2.mkString, 2))
}
