package com.kubukoz.adventofcode2017

import cats.Order
import cats.instances.int._
import cats.instances.list._
import cats.syntax.foldable._

import scala.Integral.Implicits._
import scala.Numeric.Implicits._

object Day2 {

  val parseLine: String => List[Int] = _.split("""\s+""").map(_.toInt).toList

  def diffMaxMin[T: Order : Numeric](line: List[T]): Option[T] = {
    for {
      max <- line.maximumOption
      min <- line.minimumOption
    } yield max - min
  }

  def divWhole[T: Numeric : Integral](line: List[T]): Option[T] = {
    for {
      a <- line.view
      b <- line.view
      if b != a
      if b % a == 0
    } yield b / a
  }.headOption

  def solve[LT, T: Numeric](input: List[LT])(f: LT => Option[T]): T =
    input.flatMap(f(_)).sum

  def part1[T: Order : Numeric](input: List[List[T]]): T = solve(input)(diffMaxMin(_))

  def part2[T: Numeric : Integral](input: List[List[T]]): T = solve(input)(divWhole(_))

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day2.txt").map(parseLine)

    println(part1(input)) //46402
    println(part2(input)) //265
  }
}
