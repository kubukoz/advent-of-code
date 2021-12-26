package aoc.day16

import cats.Monad
import cats.MonadError
import cats.implicits._
import cats.mtl.Stateful

trait ParserApi[F[_]] {
  def index: F[Int]
  def bit: F[Bit]
}

object ParserApi {
  def apply[F[_]](implicit F: ParserApi[F]): ParserApi[F] = F

  object ops {

    implicit class ParsersMonadicOps[F[_]: MonadError[*[_], EpsilonError]](
      private val self: ParserApi[F]
    ) {

      def const(bits: List[Bit]): F[Unit] = nBits(bits.size).flatMap {
        case actual if actual == bits => ().pure[F]
        case actual                   => raiseMessage(s"Expected $bits, got $actual")
      }

      def nBits(n: Int): F[List[Bit]] = self.bit.replicateA(n)

      def raiseMessage[A](
        msg: String
      ): F[A] = self.index.flatMap(i => EpsilonError(msg, i).raiseError[F, A])

    }

    implicit class ParserOps[F[_]: ParserApi, A](private val self: F[A]) {

      def takeThrough(
        p: A => Boolean
      )(
        implicit F: Monad[F]
      ): F[List[A]] = List.empty[A].tailRecM { memory =>
        self.map { a =>
          Either.cond(!p(a), right = (a :: memory).reverse, left = a :: memory)
        }
      }

    }

  }

  implicit def statefulParserApi[F[_]: MonadError[*[_], EpsilonError]](
    implicit S: Stateful[F, ParserState[Bit]]
  ): ParserApi[F] =
    new ParserApi[F] {
      val index: F[Int] = S.inspect(_.index)

      val bit: F[Bit] = S
        .inspect(_.proceed)
        .rethrow
        .flatMap { case (r, s) => S.set(s).as(r) }
        .map(identity(_))

    }

}
