package aoc

import aoc.lib._
import cats.implicits._

object Day8 extends App {

  // types
  case class Digit(value: Int) {
    lazy val segments = digitPatterns.map(_.swap)(this).chars.size
  }

  case class Pattern(value: Int) {
    def has(mask: Int): Boolean = (value & mask) == mask
    def chars = masks.collect { case (alias, mask) if has(mask) => alias }

    def isUnique: Boolean = {
      val len = chars.size

      digitPatterns.values.count(_.segments == len) == 1
    }

    def transcode(encoding: Encoding): Pattern = parsePattern(chars.map(encoding).mkString)

    def decode: Option[Digit] = digitPatterns.get(this)

    override def toString = s"Pattern(${chars.mkString} | 0b${value.toBinaryString} | $value)"
  }

  case class Line(examples: List[Pattern], target: List[Pattern])

  type Encoding = Map[Char, Char]

  // data
  val masks: Map[Char, Int] =
    ('a' to 'g')
      .zipWithIndex
      .map { case (alias, shift) => (alias -> (1 << shift)) }
      .toMap

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

  val allPossibleEncodings: Set[Encoding] = {
    val keys = masks.keySet.toList

    keys.toList.permutations.map { permuted =>
      permuted.zip(keys).toMap
    }
  }.toSet

  // parsing
  def parsePattern(pattern: String): Pattern = Pattern(pattern.map {
    masks(_)
  }.sum)

  val parseLine: String => Line = { case s"$examples | $target" =>
    Line(examples.split(" ").map(parsePattern).toList, target.split(" ").map(parsePattern).toList)
  }

  // solution

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

  def decode(
    line: Line,
    encoding: Encoding,
  ): List[Digit] = line.target.map(_.transcode(encoding).decode.get)

  def digitsToInt(digits: List[Digit]): Long =
    digits.reverse.zipWithIndex.map { case (dig, i) => dig.value * Math.pow(10, i) }.sum.round

  def solve(input: List[String]): (Int, Long) = {
    val decoded = input
      .map(parseLine)
      .fproduct(crack)
      .map((decode _).tupled)

    val part1 = decoded.flatten.count(Set(1, 4, 7, 8).compose[Digit](_.value))
    val part2 = decoded.map(digitsToInt).sum

    (part1, part2)
  }

  val (part1Example, part2Example) = solve(readAllLines("day8-example.txt"))
  assertEquals(part1Example, 26, "Part 1 example")
  assertEquals(part2Example, 61229, "Part 2 example")

  val (part1File, part2File) = solve(readAllLines("day8.txt"))
  assertEquals(part1File, 264, "Part 1")
  assertEquals(part2File, 1063760, "Part 2")
}
