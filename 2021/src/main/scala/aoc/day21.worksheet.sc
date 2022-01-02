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

def game[F[_]: Monad: Defer, Result](
  winningScore: Int,
  dice: Dice[F],
  summarize: F[Unit] => F[Result],
)(
  implicit S: Stateful[F, Game]
): F[Result] = {
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

  summarize {
    Defer[F].fix[Unit] { continue =>
      turn *> getPlayer.map(_.score).flatMap {
        case winner if winner >= winningScore => ().pure[F]
        case _                                => switchSides *> continue
      }
    }
  }
}

def part1(init: (Player, Player)): Int = {
  val (die, result) =
    game[StateT[State[Dice.DeterministicDie, *], Game, *], Unit](
      1000,
      Dice.deterministic,
      eff =>
        eff *>
          StateT.pure(()),
    )
      .runS(Game(init))
      .run(Dice.DeterministicDie(0, 0))
      .value

  die.rolls * result.players._2.score
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
      def next: S[Int] = fs2.Stream(1, 2, 3)
    }

  def stateTDice[F[_]: Applicative, S](base: Dice[F]): Dice[StateT[F, S, *]] =
    new Dice[StateT[F, S, *]] {
      def next: IndexedStateT[F, S, S, Int] = StateT.liftF(base.next)
    }

}

// def part2(
//   init: (Player, Player)
// ) =
//   game[StateT[fs2.Stream[fs2.Pure, *], Game, *], Long](7, Dice.stateTDice(Dice.quantumDice))
//     .runS(Game(init))
//     .map { game =>
//       Map(game.players._1.id -> 1L)
//     }
//     .compile
//     .foldMonoid

val players = parse(readAllLines("day21-example.txt"))

assertEquals(part1(players), 504972, "Part 1")

// part2(players)
