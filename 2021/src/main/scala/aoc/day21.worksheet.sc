import cats.effect.IO

import cats.Id

import scala.collection.mutable

import cats.kernel.Monoid

import cats.Eval

import cats.Applicative

import cats.data.IndexedStateT

import cats.data.StateT

import cats.data.State

import cats.Defer

import cats.mtl.Stateful

import cats.Monad

import aoc.lib._
import cats.implicits._

case class Player(position: Int, score: Int, id: String) {

  def move(steps: Int) = {
    val newPosition = (position + steps - 1) % 10 + 1
    copy(position = newPosition, score = score + newPosition)
  }

}

object Player {
  def make(initialPosition: Int, id: String) = Player(initialPosition, 0, id)
}

def parse(input: List[String]) =
  input match {
    case List(s"Player 1 starting position: $p1", s"Player 2 starting position: $p2") =>
      (Player.make(p1.toInt, "1"), Player.make(p2.toInt, "2"))
    case _ => sys.error("impossible")
  }

case class Die(value: Int, rolls: Int) {
  def next: Die = copy(value = (value + 1) % 100, rolls = rolls + 1)
}

case class Game(players: (Player, Player))

def game[F[_]: Monad: Defer](
  winningScore: Int,
  dice: Dice[F],
)(
  implicit S: Stateful[F, Game]
): F[Unit] = {
  val rollDice: F[Int] = dice.next
  val getPlayer: F[Player] = S.inspect(_.players._1)
  def setPlayer(p: Player): F[Unit] = S.modify(g => g.copy(players = g.players.leftMap(_ => p)))
  val scores: F[(Int, Int)] = S.inspect(_.players.bimap(_.score, _.score))
  val switchSides: F[Unit] = S.modify(g => g.copy(players = g.players.swap))

  val turn =
    for {
      steps <- rollDice.replicateA(3).map(_.combineAll)
      player <- getPlayer
      _ <- setPlayer(player.move(steps))
    } yield ()

  Defer[F].fix[Unit] { continue =>
    turn *> getPlayer.map(_.score).flatMap {
      case winner if winner >= winningScore => ().pure[F]
      case _                                => switchSides *> continue
    }
  }
}

def part1(init: (Player, Player)): Int = {
  val (die, result) =
    game[StateT[State[Dice.DeterministicDie, *], Game, *]](
      1000,
      Dice.deterministic,
    )
      .runS(Game(init))
      .run(Dice.DeterministicDie(0, 0))
      .value

  die.rolls * result.players._2.score
}

def scope[F[_]: Monad, S, A](sa: StateT[F, S, A]): StateT[F, S, A] = StateT { in =>
  sa.runA(in).tupleLeft(in)
}

trait Dice[F[_]] {
  def next: F[Int]
}

object Dice {
  case class DeterministicDie(value: Int, rolls: Int)

  def deterministic[F[_]: Monad](implicit S: Stateful[F, DeterministicDie]): Dice[F] =
    new Dice[F] {

      def next: F[Int] =
        S.inspect(_.value + 1) <* S.modify(die =>
          die.copy(value = (die.value + 1) % 100, rolls = die.rolls + 1)
        )

    }

  type S[A] = fs2.Stream[fs2.Pure, A]

  val quantumDice: Dice[S] =
    new Dice[S] {
      val next: S[Int] = fs2.Stream(1, 2, 3)
    }

  def stateTDice[F[_]: Applicative, S](base: Dice[F]): Dice[StateT[F, S, *]] =
    new Dice[StateT[F, S, *]] {
      def next: IndexedStateT[F, S, S, Int] = StateT.liftF(base.next)
    }

}

val winningScore = 21

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

def locally[F[_]: Monad, S, A](
  f: F[A]
)(
  implicit F: Stateful[F, S]
): F[A] = F.get.flatMap { stateBefore =>
  f <* F.set(stateBefore)
}

def game2[F[_]: Monad](
  cache: F[Map[String, Long]] => F[Map[String, Long]]
)(
  implicit S: Stateful[F, Game]
): F[Map[String, Long]] = cache {
  val getPlayer: F[Player] = S.inspect(_.players._1)
  def setPlayer(p: Player): F[Unit] = S.modify(g => g.copy(players = g.players.leftMap(_ => p)))
  val scores: F[(Int, Int)] = S.inspect(_.players.bimap(_.score, _.score))
  val switchSides: F[Unit] = S.modify(g => g.copy(players = g.players.swap))

  List(1, 2, 3)
    .pure[F]
    .flatMap {
      _.replicateA(3)
        .map(_.combineAll)
        .foldMapM { steps =>
          val step =
            for {
              player <- getPlayer
              _ <- setPlayer(player.move(steps))
            } yield ()

          locally {
            step *> getPlayer
              .map(_.score)
              .flatMap {
                case winner if winner >= winningScore => getPlayer.map(p => Map(p.id -> 1L))
                case _                                => switchSides *> game2[F](cache)
              }
          }
        }
    }
}

def part2(init: (Player, Player)): Long =
  game2[State[Game, *]](cached).runA(Game(init)).value.map(_._2).max

val example = parse(readAllLines("day21-example.txt"))
val data = parse(readAllLines("day21.txt"))

assertEquals(part1(example), 739785, "Part 1")
assertEquals(part1(data), 504972, "Part 1")

// assertEquals(part2(example), 444356092776315L, "Part 2 example")
// assertEquals(part2(data), 446968027750017L, "Part 2")
