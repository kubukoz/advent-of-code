package aoc.day18

import cats.Defer
import cats.Eval
import cats.MonadError
import cats.data.EitherT
import cats.data.StateT
import cats.implicits._
import cats.mtl.Stateful

trait Parser[F[_]] {
  def char: F[Char]
  def fail[A](msg: String): F[A]
}

object Parser {
  def apply[F[_]](implicit F: Parser[F]): Parser[F] = F

  implicit def parserStateful[F[_]: MonadError[*[_], Throwable]](
    implicit S: Stateful[F, (String, Int)]
  ): Parser[F] =
    new Parser[F] {
      def char: F[Char] = S.inspect { case (s, i) => s(i) } <* S.modify(_.map(_ + 1))

      def fail[A](msg: String): F[A] = S
        .inspect(_._2)
        .map(i => new Throwable(s"$msg at index $i"))
        .flatMap(_.raiseError[F, A])

    }

  def parseFull[F[_]: Parser: Defer](
    implicit F: MonadError[F, Throwable]
  ): F[Snailfish] = Defer[F].fix { self =>
    val api = Parser[F]
    import api._

    def const(ch: Char): F[Unit] = char.flatMap {
      case `ch`   => ().pure[F]
      case actual => fail(s"expected $ch, got $actual")
    }

    def parseInt(ch: Char): F[Int] =
      ch.toString.toIntOption match {
        case Some(v) => v.pure[F]
        case None    => fail[Int]("expected digit")
      }

    val leaf: F[Snailfish] =
      (
        char.flatMap(parseInt)
      ).map(Leaf.apply)

    val pair: F[Snailfish] =
      (
        const('[') *> self <* const(','),
        self <* const(']'),
      ).mapN(Pair.apply)

    leaf.orElse(pair)
  }

  def parsePair(
    s: String
  ): Snailfish =
    parseFull[StateT[EitherT[Eval, Throwable, *], (String, Int), *]]
      .runA((s, 0))
      .value
      .value
      .toTry
      .get

}
