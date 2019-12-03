package com.kubukoz.adventofcode2017

import fastparse.all._

object Day21 {

  case class Matrix[T](values: Seq[Seq[T]]) extends AnyVal {
    def regions: Matrix[Matrix[T]] = {
      def goRegions(size: Int): Matrix[Matrix[T]] = {
        Matrix {
          values.grouped(size)
            .map { rows =>
              val amount = rows.head.size / size

              val rowGroups: Seq[Int => Seq[T]] = rows.map { row =>
                (i: Int) => row.slice(i, i + size)
              }

              (0 until amount).map(_ * size).map { offset =>
                Matrix(rowGroups.map(_ (offset)))
              }
            }.toSeq
        }
      }

      values.size match {
        case s if s % 2 == 0 => goRegions(2)
        case s if s % 3 == 0 => goRegions(3)
      }
    }

    def flatMap[U](f: T => Matrix[U]): Matrix[U] = {
      Matrix {
        values.flatMap {
          _.map(f).reduceLeft(_ mergeHorizontal _).values
        }
      }
    }

    def mirrored: Seq[Matrix[T]] = {
      val hor = Matrix(values.map(_.reverse))
      val ver = Matrix(values.reverse)
      hor :: ver :: Nil
    }

    def rotated: Matrix[T] = Matrix(values.transpose.map(_.reverse))

    def mergeHorizontal(another: Matrix[T]): Matrix[T] = {
      Matrix((values zip another.values).map {
        case (a, b) => a ++ b
      })
    }
  }

  type State = Matrix[Boolean]

  object State {
    def parse(s: String, lineSep: String): State = stateParser(lineSep).parse(s).get.value
  }

  case class Rule(from: State, to: State) {
    val permutations: Set[State] = {
      Seq.iterate(from, 4)(_.rotated)
        .flatMap(x => x.mirrored :+ x).toSet
    }
  }

  def transformAndCount(initialState: State, rules: Seq[Rule], iterations: Int): Int = {
    def transformOnce(state: State): State = {
      state.regions.flatMap { region =>
        rules.find(_.permutations(region)).map(_.to).get
      }
    }

    val str = Stream.iterate(initialState, iterations + 1)(transformOnce)
    str.last.values.map(_.count(true == _)).sum
  }


  def stateParser(lineSep: String): Parser[State] = {
    val bit: Parser[Boolean] = P(
      "#".!.map(_ => true) | ".".!.map(_ => false)
    )

    P(bit.rep(1).rep(1, sep = lineSep)).map(Matrix(_))
  }

  def ruleParser(lineSep: String): Parser[Rule] = {
    val state = stateParser(lineSep)

    P(state ~ " => " ~ state).map(Rule.tupled)
  }

  def parseLine(s: String): Rule = ruleParser("/").parse(s).get.value

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day21.txt").map(parseLine)

    val start = stateParser("\n").parse(
      """.#.
        |..#
        |###""".stripMargin
    ).get.value

    println(transformAndCount(start, input, 5))
    println(transformAndCount(start, input, 18))
  }
}
