package aoc

import aoc.lib._
import cats.Applicative
import cats.Defer
import cats.Monad
import cats.data.State
import cats.data.StateT
import cats.implicits._
import cats.kernel.Monoid
import cats.mtl.Stateful

import scala.collection.mutable

object Day21 extends App {

  case class Player(position: Int, score: Int, id: String) {

    def move(steps: Int) = {
      val newPosition = (position + steps - 1) % 10 + 1
      copy(position = newPosition, score = score + newPosition)
    }

  }

  object Player {
    def make(initialPosition: Int, id: String) = Player(initialPosition, 0, id)
  }

  case class Die(value: Int, rolls: Int) {
    def next: Die = copy(value = (value + 1) % 100, rolls = rolls + 1)
  }

  case class Game(players: (Player, Player))

  // util
  def cached[F[_]: Monad, S, A](implicit F: Stateful[F, S]): F[A] => F[A] = {
    val cache: mutable.Map[S, (S, A)] = mutable.Map.empty

    eff =>
      F.get.flatMap { s =>
        cache.get(s) match {
          case None =>
            eff.flatTap { result =>
              F.get.map { resultState =>
                cache.put(s, (resultState, result))
              }
            }
          case Some((newState, v)) => F.set(newState).as(v)
        }
      }
  }

  def scopedStateful[F[_]: Monad, S, A](
    f: F[A]
  )(
    implicit F: Stateful[F, S]
  ): F[A] = F.get.flatMap(stateBefore => f <* F.set(stateBefore))

  // game logic

  def getPlayer[F[_]](implicit S: Stateful[F, Game]): F[Player] = S.inspect(_.players._1)

  def game[F[_]: Monad: Defer, Result: Monoid](
    winningScore: Int,
    cache: F[Result] => F[Result],
    nextSteps: F[List[Int]],
    mkResult: F[Result],
  )(
    implicit S: Stateful[F, Game]
  ): F[Result] = Defer[F].fix { recurse =>
    cache {
      def setPlayer(p: Player): F[Unit] = S.modify(g => g.copy(players = g.players.leftMap(_ => p)))
      val scores: F[(Int, Int)] = S.inspect(_.players.bimap(_.score, _.score))
      val switchSides: F[Unit] = S.modify(g => g.copy(players = g.players.swap))

      nextSteps
        .flatMap {
          _.foldMapM { steps =>
            val step =
              for {
                player <- getPlayer
                _ <- setPlayer(player.move(steps))
              } yield ()

            scopedStateful {
              step *> getPlayer
                .map(_.score)
                .flatMap {
                  case winner if winner >= winningScore => mkResult
                  case _                                => switchSides *> recurse
                }
            }
          }
        }
    }
  }

  object Dice {

    case class DeterministicDie(value: Int, rolls: Int) {
      def roll: DeterministicDie = copy(value = (value + 1) % 100, rolls = rolls + 1)
    }

    def deterministic[F[_]: Monad](implicit S: Stateful[F, DeterministicDie]): F[List[Int]] = {
      S.inspect(_.value + 1) <*
        S.modify(_.roll)
    }.replicateA(3).map(_.combineAll).map(List(_))

    def quantumDice[
      F[_]: Applicative
    ]: F[List[Int]] = List(1, 2, 3).replicateA(3).map(_.combineAll).pure[F]

  }
  // solutions

  def part1(init: Game): Int = {
    type F[A] = StateT[State[Dice.DeterministicDie, *], Game, A]

    val getDie: F[Dice.DeterministicDie] = StateT.liftF(State.get)
    val getLoser: F[Player] = StateT.inspect(_.players._2)

    game[F, Int](
      winningScore = 1000,
      cache = cached,
      nextSteps = Dice.deterministic[F],
      mkResult =
        (
          getDie.map(_.rolls),
          getLoser.map(_.score),
        ).mapN(_ * _),
    )
      .runA(init)
      .runA(Dice.DeterministicDie(0, 0))
      .value
  }

  def part2(init: Game): Long = {
    type F[A] = State[Game, A]

    game[F, Map[String, Long]](
      winningScore = 21,
      cache = cached,
      nextSteps = Dice.quantumDice[F],
      mkResult = getPlayer[F].map(p => Map(p.id -> 1L)),
    )
      .runA(init)
      .value
      .map(_._2)
      .max
  }

  def parse(input: List[String]): Game =
    input match {
      case List(s"Player 1 starting position: $p1", s"Player 2 starting position: $p2") =>
        Game(
          Player.make(p1.toInt, "1"),
          Player.make(p2.toInt, "2"),
        )
      case _ => sys.error("impossible")
    }

  val example = parse(readAllLines("day21-example.txt"))
  val data = parse(readAllLines("day21.txt"))

  locally {
    assertEquals(part1(example), 739785, "Part 1 example")
    assertEquals(part2(example), 444356092776315L, "Part 2 example")
  }

  locally {
    assertEquals(part1(data), 504972, "Part 1")
    assertEquals(part2(data), 446968027750017L, "Part 2")
  }
}
