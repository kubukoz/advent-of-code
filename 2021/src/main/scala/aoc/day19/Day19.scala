package aoc.day19

import aoc.lib._
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

object Day19 extends App {

  def parseScanner(text: String) =
    text match {
      case s"--- scanner $id ---$lines" =>
        ScannerDescriptor(
          id,
          lines
            .trim
            .split("\n")
            .map { case s"$x,$y,$z" => Position(x.toInt, y.toInt, z.toInt) }
            .toList,
          Position.init,
        )
    }

  def parse(text: String) = text.split("\n\n").map(parseScanner).toList

  def areMatching(
    s1: ScannerDescriptor,
    s2: ScannerDescriptor,
  ) = s1.positions.intersect(s2.positions).sizeIs >= 12

  // Pairs of scanners' IDs that have already been proven to have insufficient overlap
  // For my input this goes up to 364 in size, so it fills up pretty fast. Speedup is ~10x
  private val mismatches: mutable.Set[(String, String)] = mutable.Set.empty

  def resolveOverlap(s1: ScannerDescriptor, s2: ScannerDescriptor) =
    if (mismatches.contains((s1.id, s2.id)))
      None
    else {
      val result = s1
        .relative
        .collectFirstSome { group1 =>
          s2
            .permute
            .to(LazyList)
            .flatMap(_.relative)
            .collectFirst {
              case group2 if areMatching(group1, group2) => (group1, group2)
            }
        }

      if (result.isEmpty)
        mismatches.add((s1.id, s2.id))

      result
    }

  def resolve(
    scanners: List[ScannerDescriptor]
  ): List[ScannerDescriptor] = {
    @tailrec
    def go(
      remaining: Map[String, ScannerDescriptor],
      resolved: List[ScannerDescriptor],
    ): List[ScannerDescriptor] = {
      println("remaining: " + remaining.size + ", cache size " + mismatches.size)
      if (remaining.isEmpty)
        resolved
      else {
        val newMatch = remaining
          .values
          .to(LazyList)
          .collectFirstSome { current =>
            resolved
              .collectFirstSome { seek =>
                resolveOverlap(seek, current)
                  .map { case (seekMoved, found) =>
                    // The movement of `seek` while resolving the overlap.
                    // This can probably be retrieved from `seekMoved` now instead of recalculating.
                    val movement = seekMoved.positions.head |-| seek.positions.head

                    found.relativeTo(movement)
                  }
              }
          }
          .getOrElse(sys.error("No overlap found"))

        go(remaining - newMatch.id, newMatch :: resolved)
      }
    }

    val scannersGrouped = scanners.tail.groupByNel(_.id).map(_.map(_.head))

    scanners
      .tail
      .collectFirstSome {
        resolveOverlap(scanners.head, _)
      } match {
      case Some((s1, s2)) => go(scannersGrouped - s2.id, List(s1, s2))
      case None           => sys.error("No overlap found")
    }
  }

  val input = parse(readAll("day19.txt"))

  val resolved = resolve(input)
  val part1 = resolved.foldMap(_.positions.toSet).size

  val part2 =
    resolve(input)
      .map(_.movement)
      .combinations(2)
      .map {
        case a :: b :: Nil => a.distance(b)
        case _             => sys.error("impossible")
      }
      .max

}
