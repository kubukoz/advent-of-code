package aoc.day18

import aoc.lib._
import cats.implicits._

object Day18 extends App {

  object Snailfish {

    def replaceNextIn(it: WithPath[RawPair[Int]], direction: Direction): Snailfish => Snailfish =
      BinaryTree
        .next[Int](it.map(_.asTree), direction)
        .modify(_.map(_ + it.content.select(direction)))

    def rawExplode(
      it: WithPath[RawPair[Int]]
    ): Snailfish => Snailfish = BinaryTree.at(it.path).replace(Leaf(0))

    def tryExplode(root: Snailfish): Option[Snailfish] = root
      .collectFirst { self =>
        self.traverse(_.asRawPair.filter(_ => self.path.size >= 4))
      }
      .map { it =>
        replaceNextIn(it, Left)
          .andThen(replaceNextIn(it, Right))
          .andThen(rawExplode(it))
          .apply(root)
      }

    def trySplit(root: Snailfish): Option[Snailfish] = root
      .collectFirst { self =>
        self.traverse(_.asLeaf).filter(_.content.n >= 10)
      }
      .map { it =>
        val n = it.content.n
        val low = n / 2
        val high = (n / 2.0f).round

        root.replaceAt(it.path, Pair(Leaf(low), Leaf(high)))
      }

    implicit class SnailfishOps(root: Snailfish) {

      def reduce: Snailfish = tryExplode(root)
        .orElse(trySplit(root))
        .map(_.reduce)
        .getOrElse(root)

    }

  }

  import Snailfish._

  def part1(data: List[Snailfish]) = data.reduceLeft(Pair(_, _).reduce).magnitude

  def part2(data: List[Snailfish]) =
    (data, data)
      .tupled
      .to(LazyList)
      .filterNot { case (a, b) => a == b }
      .map { case (a, b) => Pair(a, b).reduce.magnitude }
      .max

  import Parser.parsePair
  locally {
    val example = readAllLines("day18-example.txt")

    assertEquals(part1(example.map(parsePair)), 4140, "Part 1 (example)")
    assertEquals(part2(example.map(parsePair)), 3993, "Part 2 (example)")
  }

  locally {
    val fromFile = readAllLines("day18.txt")

    assertEquals(part1(fromFile.map(parsePair)), 2541, "Part 1")
    assertEquals(part2(fromFile.map(parsePair)), 4647, "Part 2")
  }
}
