package com.kubukoz.adventofcode2017
import fastparse.all._

object Day20 {

  case class Coords(x: Long, y: Long, z: Long) {
    def +(another: Coords) = Coords(x + another.x, y + another.y, z + another.z)
    def sum: Long = x.abs + y.abs + z.abs
  }

  case class Info(position: Coords, velocity: Coords, acceleration: Coords)
  val p = "p=<-435,1273,-382>, v=<-7,-121,-13>, a=<6,3,6>"
  val number: Parser[Long] = P(
    "-".? ~ CharIn('0' to '9').rep(min = 1)
  ).!.map(_.toLong)

  val coords: Parser[Coords] = P(
    "<" ~ number ~ "," ~ number ~ "," ~ number ~ ">"
  ).map(Coords.tupled)

  val info: Parser[Info] =
    P("p=" ~ coords ~ ", v=" ~ coords ~ ", a=" ~ coords).map(Info.tupled)

  val updateAll: List[Info] => List[Info] = _.map { i =>
    val newVelo = i.velocity + i.acceleration

    Info(i.position + newVelo, newVelo, i.acceleration)
  }

  def solve1(parsed: List[Info]): Int = {

    Stream.iterate(parsed)(updateAll)
      .map(_.zipWithIndex.minBy(_._1.position.sum))
      .take(250).last._2
  }

  def solve2(parsed: List[Info]): Int = {
    val removeCollisions: List[Info] => List[Info] =
      _.groupBy(_.position)
        .values.filter(_.size == 1)
        .flatten.toList

    Stream.iterate(parsed)(updateAll andThen removeCollisions)
      .take(50).last.size
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day20.txt")
    val parsed = input.map(info.parse(_).get.value)

    println(solve1(parsed))
    println(solve2(parsed))
  }
}
