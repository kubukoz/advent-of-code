import java.lang

import aoc.lib._

import cats.implicits._

val input = readAllLines("day8.txt")
// val input = List(
//   "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
// )

val start = lang.System.nanoTime()

val masks: Map[Char, Int] =
  ('a' to 'g')
    .zipWithIndex
    .map { case (alias, shift) => (alias -> (1 << shift)) }
    .toMap

case class Digit(value: Int) {
  lazy val segments = digitPatterns.map(_.swap)(this).chars.size
}

val digitPatterns: Map[Pattern, Digit] = Map(
  0 -> "abcefg",
  1 -> "cf",
  2 -> "acdeg",
  3 -> "acdfg",
  4 -> "bcdf",
  5 -> "abdfg",
  6 -> "abdefg",
  7 -> "acf",
  8 -> "abcdefg",
  9 -> "abcdfg",
).map(_.map(parsePattern).leftMap(Digit(_)).swap)

case class Pattern(value: Int) {
  def has(mask: Int): Boolean = (value & mask) == mask
  def chars = masks.collect { case (alias, mask) if has(mask) => alias }

  def isUnique: Boolean = {
    val len = chars.size

    digitPatterns.values.count(_.segments == len) == 1
  }

  // todo optimize?
  def transcode(encoding: Encoding): Pattern = parsePattern(chars.map(encoding).mkString)

  def decode: Option[Digit] = digitPatterns.get(this)

  override def toString = s"Pattern(${chars.mkString} | 0b${value.toBinaryString} | $value)"
}

def parsePattern(pattern: String): Pattern = Pattern(pattern.map {
  masks(_)
}.sum)

case class Line(examples: List[Pattern], target: List[Pattern])

val parseLine: String => Line = { case s"$examples | $target" =>
  Line(examples.split(" ").map(parsePattern).toList, target.split(" ").map(parsePattern).toList)
}

val data = input.map(parseLine)

type Encoding = Map[Char, Char]

val allPossibleEncodings: Set[Encoding] = {
  val keys = masks.keySet.toList

  keys.toList.permutations.map { permuted =>
    permuted.zip(keys).toMap
  }
}.toSet

def minimizeEncodings(possibleEncodings: Set[Encoding], encoded: Pattern): Set[Encoding] =
  possibleEncodings.filter { enc =>
    encoded.transcode(enc).decode.isDefined
  }

def crack(line: Line): Encoding = {

  val (uniqueExamples, complexExamples) = (line.examples ++ line.target).partition(_.isUnique)

  val ordered = uniqueExamples.toList ++ complexExamples.toList

  val encoding = ordered.foldLeft(allPossibleEncodings)(minimizeEncodings)

  require(encoding.size == 1)

  encoding.head
}

def digitsToInt(digits: List[Digit]): Long =
  digits.reverse.zipWithIndex.map { case (dig, i) => dig.value * Math.pow(10, i) }.sum.round

data
  .map { line =>
    val encoding = crack(line)

    line.target.map(_.transcode(encoding).decode.get)
  }
  // part 1
  // .flatten
  // .count(d => Set(1, 4, 7, 8).contains(d.value))
  // part 2
  .map(digitsToInt)
  .sum

//
//
//
//
//
//
//
//
val end = lang.System.nanoTime()
import scala.concurrent.duration._
(end - start).nanos.toMillis.toString + "ms"
// }
