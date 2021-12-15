package aoc

import cats.Defer
import cats.Monad
import cats.data.OptionT
import cats.data.State
import cats.mtl.Stateful
import cats.mtl.Ask
import cats.data.Kleisli

object Day14 extends App {
  import lib._

  import cats.implicits._

  type K = (List[Char], Int)
  type Result = Map[Char, Long]

  def cachedF[F[_]: Monad, K, V](calc: K => F[V])(implicit S: Stateful[F, Map[K, V]]): K => F[V] =
    k => {
      def write(v: V): F[Unit] = S.modify(_ + (k -> v))

      OptionT(S.inspect(_.get(k))).getOrElseF(calc(k).flatTap(write))
    }

  type Rules = Map[(Char, Char), Char]

  def expand[F[_]](left: Char, right: Char)(implicit F: Ask[F, Rules]) = F.reader { rules =>
    left :: rules(left, right) :: right :: Nil
  }

  def unfold[F[_]: Monad: Defer](
    implicit S: Stateful[F, Map[K, Result]],
    R: Ask[F, Rules],
  ): K => F[Result] = cachedF { case (base, steps) =>
    // actually stack safe thanks to trampolining
    def recurseInternal(remaining: List[Char], memory: Result): F[Result] =
      remaining match {
        case one :: two :: rest =>
          expand[F](one, two)
            .flatMap(unfold[F].apply(_, steps - 1))
            .flatMap { mergeInit =>
              rest match {
                case Nil => (memory |+| mergeInit).pure[F]
                case _ :: _ =>
                  val updatedResult = mergeInit.updatedWith(two)(_.map(_ - 1))

                  recurseInternal(two :: rest, updatedResult |+| memory)
              }
            }

        case h :: Nil => Map(h -> 1L).pure[F]
        case Nil      => (Map.empty: Result).pure[F]
      }

    if (steps == 0)
      base.groupBy(identity).map(_.map(_.size.toLong)).pure[F]
    else {
      recurseInternal(base, Map.empty)
    }
  }

  def solve(rounds: Int): List[String] => Result = { input =>
    val template = input.head.toList

    val rules: Rules =
      input
        .drop(2)
        .map { case s"$lhs -> $i" => ((lhs.head, lhs(1)), i.head) }
        .toMap

    unfold[Kleisli[State[Map[K, Result], *], Rules, *]]
      .apply((template, rounds))
      .run(rules) // Kleisli
      .runA(Map.empty) // StateT
      .value // Eval
  }

  def summarize(result: Result): Long = {
    val counts = result.values

    counts.max - counts.min
  }

  val part1 = solve(10).andThen(summarize)
  val part2 = solve(40).andThen(summarize)

  val example = readAllLines("day14-example.txt")

  assertEquals(part1(example), 1588L, "Part 1 (example)")
  assertEquals(part2(example), 2188189693529L, "Part 2 (example)")

  val fromFile = readAllLines("day14.txt")

  assertEquals(part1(fromFile), 3697L, "Part 1")
  assertEquals(part2(fromFile), 4371307836157L, "Part 2")
}
