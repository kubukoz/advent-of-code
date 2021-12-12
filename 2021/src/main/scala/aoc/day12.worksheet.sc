import cats.data.NonEmptyList

import aoc.lib._
import cats.implicits._

val input = readAllLines("day12.txt")
// val input = readAllLines("day12-example.txt")

case class Edge(from: String, to: String)

val isStart: String => Boolean = _ == "start"
val isEnd: String => Boolean = _ == "end"
val isSmall: String => Boolean = _.matches("[a-z]+")

val edges =
  input
    .map(_.split("-") match {
      case Array(a, b) => Edge(a, b)
    })
    .toSet

val edgesByFrom = edges.groupBy(_.from).map(_.map(_.map(_.to)))
val edgesByTo = edges.groupBy(_.to).map(_.map(_.map(_.from)))

val edgesBoth = edgesByFrom |+| edgesByTo

def findPaths() = {
  def go(history: NonEmptyList[String]): NonEmptyList[NonEmptyList[String]] = {
    val possibleNext =
      edgesBoth.getOrElse(history.head, Set.empty) --
        history.filter(isSmall)

    if (possibleNext.isEmpty)
      NonEmptyList.one(history)
    else {

      NonEmptyList(
        history,
        possibleNext.toList.flatMap { next =>
          go(next :: history).toList
        },
      )
    }
  }

  go(NonEmptyList.of("start"))
    .filter(path => isEnd(path.head))
    .map(_.reverse)
}

// findPaths().foreach(println)
findPaths().size
