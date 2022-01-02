package aoc.day19

import aoc.lib._
import cats.implicits._

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
        )
    }

  def parse(text: String) = text.split("\n\n").map(parseScanner).toList

  def resolveOverlap(s1: ScannerDescriptor, s2: ScannerDescriptor) = s1
    .relative
    .collectFirstSome { case (d1, group1) =>
      s2
        .permute
        .flatMap(_.relative)
        .collectFirst {
          case (d2, group2) if (group1.positions.intersect(group2.positions)).sizeIs >= 12 =>
            (group1, group2)
        }
    }

  def resolve(
    scanners: List[ScannerDescriptor]
  ): List[ScannerDescriptor] = {
    def go(
      remaining: Map[String, ScannerDescriptor],
      resolved: List[ScannerDescriptor],
    ): List[ScannerDescriptor] = {
      println("remaining: " + remaining.size)
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

  println(resolve(input).foldMap(_.positions.toSet).size)
}
