package com.kubukoz.aoc.day6

import cats.effect.{ExitCode, IO, IOApp}
import cats.kernel.Semigroup
import io.estatico.newtype.macros.newtype
import cats.implicits._
import cats.effect.Console.io._
import com.kubukoz.aoc.Util
import com.kubukoz.aoc.day6.data.{DirectOrbit, Orb, Tree}

private[day6] object data {

  @newtype
  final case class Orb(name: String)
  final case class DirectOrbit(orbitted: Orb, orbitter: Orb)

  final case class Tree[T](head: T, children: List[Tree[T]]) {
    def leafDistances(n: Int): List[Int] = List(n) ::: children.flatMap(_.leafDistances(n + 1))
  }
}

object Day6 extends IOApp {

  val input = """COM)B
                |B)C
                |C)D
                |D)E
                |E)F
                |B)G
                |G)H
                |D)I
                |E)J
                |J)K
                |K)L
                |K)YOU
                |I)SAN""".stripMargin

  val parseLine: String => DirectOrbit = _.split("\\)") match {
    case Array(a, b) => DirectOrbit(Orb(a), Orb(b))
  }

  def toTree(orbits: List[DirectOrbit]): Tree[Orb] = {
    val roots     = orbits.map(_.orbitted).toSet
    val orbitters = orbits.map(_.orbitter).toSet

    def buildTree(root: Orb): Tree[Orb] = {
      val directOrbitters = orbits.to(LazyList).filter(_.orbitted == root).map(_.orbitter)

      Tree(root, directOrbitters.map(buildTree).toList)
    }

    val topLevel = roots.find(!orbitters(_)).get

    buildTree(topLevel)
  }

  def run(args: List[String]): IO[ExitCode] =
    Util.readFile[IO]("files/day6.txt").flatMap { fileInput =>
//      val orbits    = fileInput.linesIterator.map(parseLine).toList
      val orbits    = input.linesIterator.map(parseLine).toList
      val orbitTree = toTree(orbits)

      val orbitCount = orbitTree.leafDistances(0).sum

      putStrLn(
        orbitCount
      )
    } as ExitCode.Success
}
