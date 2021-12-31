import cats.data.Writer

import cats.Apply

import cats.NonEmptyTraverse

import cats.Functor

import cats.data.NonEmptyList

import cats.data.Chain

import scala.collection.immutable

import cats.Eval

import cats.data.EitherT

import cats.mtl.Stateful

import cats.data.StateT

import cats.Defer

import cats.MonadError

import aoc.lib._
import cats.implicits._
val fromFile = readAllLines("day18.txt")

val input = fromFile

case class RawPair[A](left: Number[A], right: Number[A]) {
  def asTree: Tree[A] = Pair(left, right)
}

sealed trait Tree[A] {

  def render: String =
    this match {
      case Number(n)      => n.toString()
      case Pair(lhs, rhs) => s"[${lhs.render},${rhs.render}]"
    }

  def down(dir: Direction): Tree[A] =
    this match {
      case Number(n)      => sys.error("can't go down on number")
      case p @ Pair(_, _) => p.select(dir)
    }

  def getAt(path: Chain[Direction]): Tree[A] = path.toList.foldLeft(this)(_.down(_))

  def replaceAt(path: Chain[Direction], newValue: Tree[A]): Tree[A] = {
    def go(rest: List[Direction], self: Tree[A]): Tree[A] =
      rest match {
        case Nil      => sys.error("oops")
        case h :: Nil => self.downReplace(h, newValue)
        case h :: more =>
          self.downReplace(
            // replace at this direction
            h,
            // use remainder on path, element is the current element at the direction
            go(more, self.down(h)),
          )
      }

    go(path.toList, this)
  }

  def downReplace(dir: Direction, newValue: Tree[A]): Tree[A] =
    this match {
      case Number(n)      => sys.error("can't go down on number")
      case p @ Pair(_, _) => p.insertAt(dir, newValue)
    }

  def collectFirst[B](
    withNode: WithPath[Tree[A]] => Option[B],
    pathSoFar: Chain[Direction] = Chain.nil,
  ): Option[B] = withNode(WithPath(this, pathSoFar)).orElse {
    this match {
      case Pair(lhs, rhs) =>
        lhs
          .collectFirst(withNode, pathSoFar.append(Left))
          .orElse(rhs.collectFirst(withNode, pathSoFar.append(Right)))
      case _ => None
    }
  }

  def collectLast[B](
    withNode: WithPath[Tree[A]] => Option[B],
    pathSoFar: Chain[Direction] = Chain.nil,
  ): Option[B] = {
    this match {
      case self @ Pair(lhs, rhs) =>
        rhs
          .collectLast(withNode, pathSoFar.append(Right))
          .orElse(lhs.collectLast(withNode, pathSoFar.append(Left)))
      case _ => None
    }
  }.orElse(withNode(WithPath(this, pathSoFar)))

  def asLeaf: Option[Number[A]] =
    this match {
      case n @ Number(_) => Some(n)
      case _             => None
    }

  def asPair: Option[Pair[A]] =
    this match {
      case p @ Pair(_, _) => Some(p)
      case _              => None
    }

  def asRawPair: Option[RawPair[A]] =
    this match {
      case Pair(n1: Number[a], n2: Number[b]) => Some(RawPair(n1, n2))
      case _                                  => None
    }

}

sealed trait Direction {

  def flip: Direction =
    this match {
      case Left  => Right
      case Right => Left
    }

}

case object Left extends Direction
case object Right extends Direction

case class WithPath[A](content: A, path: Chain[Direction]) {

  // yo... this is writer
  def flatMap[B](f: A => WithPath[B]): WithPath[B] = {
    val r = f(content)
    WithPath(r.content, path ++ r.path)
  }

}

implicit val withPathTraversable: NonEmptyTraverse[WithPath] =
  new NonEmptyTraverse[WithPath] {
    def foldLeft[A, B](fa: WithPath[A], b: B)(f: (B, A) => B): B = f(b, fa.content)

    def foldRight[A, B](
      fa: WithPath[A],
      lb: Eval[B],
    )(
      f: (A, Eval[B]) => Eval[B]
    ): Eval[B] = f(fa.content, lb)

    def reduceLeftTo[A, B](fa: WithPath[A])(f: A => B)(g: (B, A) => B): B = f(fa.content)

    def reduceRightTo[A, B](
      fa: WithPath[A]
    )(
      f: A => B
    )(
      g: (A, Eval[B]) => Eval[B]
    ): Eval[B] = Eval.later(f(fa.content))

    def nonEmptyTraverse[G[_]: Apply, A, B](
      fa: WithPath[A]
    )(
      f: A => G[B]
    ): G[WithPath[B]] = f(fa.content).map(WithPath(_, fa.path))

  }

case class Number[A](n: A) extends Tree[A]

case class Pair[A](lhs: Tree[A], rhs: Tree[A]) extends Tree[A] {

  def select(dir: Direction): Tree[A] =
    dir match {
      case Left  => lhs
      case Right => rhs
    }

  def insertAt(dir: Direction, value: Tree[A]): Tree[A] =
    dir match {
      case Left  => copy(lhs = value)
      case Right => copy(rhs = value)
    }

}

type Snailfish = Tree[Int]

trait Parser[F[_]] {
  def char: F[Char]
  def fail[A](msg: String): F[A]
}

object Parser {
  def apply[F[_]](implicit F: Parser[F]): Parser[F] = F
}

def parseFull[F[_]: Parser: Defer](
  path: Chain[Direction]
)(
  implicit F: MonadError[F, Throwable]
): F[Snailfish] = Defer[F].defer {
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

  val number: F[Snailfish] =
    (
      char.flatMap(parseInt)
    ).map(Number.apply)

  val pair: F[Snailfish] =
    (
      const('[') *> parseFull(path.append(Left)) <* const(','),
      parseFull(path.append(Right)) <* const(']'),
    ).mapN(Pair.apply)

  number.orElse(pair)
}

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

def parsePair(
  s: String
): Snailfish =
  parseFull[StateT[EitherT[Eval, Throwable, *], (String, Int), *]](Chain.nil)
    .runA((s, 0))
    .value
    .value
    .toTry
    .get

val data = fromFile.map(parsePair)

// val root = parsePair("[[[[[9,8],1],2],3],4]")
val root = parsePair("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")

val it =
  root.collectFirst { self =>
    self.traverse(_.asRawPair.filter(_ => self.path.size >= 4))
  }.get

def findNext(
  root: Snailfish,
  self: WithPath[Snailfish],
  direction: Direction,
): Option[WithPath[Snailfish]] =
  // we wanna go left - drop everything including the first Right (from the end), reverse again and add Left
  self.path.toList.reverse.dropWhile(_ == direction) match {
    case _ :: t =>
      val newPath = Chain
        .fromSeq(t.reverse)
        .append(direction)
      WithPath(root.getAt(newPath), newPath).some
    case _ => None
  }

it

it.content.left.n
it.content.right.n
it.path.toList

val nextBefore =
  findNext(root, it.map(_.asTree), Left)
    .map(_.flatMap(_.collectLast(_.traverse(_.asLeaf)).get))
    .get

val afterLeftReplace = root
  .replaceAt(nextBefore.path, Number(nextBefore.content.n + it.content.left.n))

val nextAfter =
  findNext(root, it.map(_.asTree), Right)
    .map(_.flatMap(_.collectFirst(_.traverse(_.asLeaf)).get))
    .get

val afterRightReplace = afterLeftReplace
  .replaceAt(nextAfter.path, Number(nextAfter.content.n + it.content.right.n))

val afterAllReplacements = afterRightReplace
  .replaceAt(it.path, Number(0))

root.render
afterRightReplace.render
afterAllReplacements.render == "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
