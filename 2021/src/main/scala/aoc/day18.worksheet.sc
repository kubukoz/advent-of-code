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

case class RawPair[A](left: Number[A], right: Number[A], path: Chain[Direction]) {
  def asTree: Tree[A] = Pair(left, right, path)
}

sealed trait Tree[A] {

  def render: String =
    this match {
      case Number(n, path)      => n.toString()
      case Pair(lhs, rhs, path) => s"[${lhs.render},${rhs.render}]"
    }

  def withPathPrefix(prefix: Chain[Direction]): Tree[A]

  def down(dir: Direction): Tree[A] =
    this match {
      case Number(n, path)   => sys.error("can't go down on number")
      case p @ Pair(_, _, _) => p.select(dir)
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
      case Number(n, path)   => sys.error("can't go down on number")
      case p @ Pair(_, _, _) => p.insertAt(dir, newValue)
    }

  def path: Chain[Direction]

  def collectFirst[B](
    withNode: Tree[A] => Option[B]
  ): Option[B] = withNode(this).orElse {
    this match {
      case Pair(lhs, rhs, p) =>
        lhs
          .collectFirst(withNode)
          .orElse(rhs.collectFirst(withNode))
      case _ => None
    }
  }

  def collectLast[B](
    withNode: Tree[A] => Option[B]
  ): Option[B] = {
    this match {
      case self @ Pair(lhs, rhs, _) =>
        rhs
          .collectLast(withNode)
          .orElse(lhs.collectLast(withNode))
      case _ => None
    }
  }.orElse(withNode(this))

  def asLeaf: Option[Number[A]] =
    this match {
      case n @ Number(_, _) => Some(n)
      case _                => None
    }

  def asPair: Option[Pair[A]] =
    this match {
      case p @ Pair(_, _, _) => Some(p)
      case _                 => None
    }

  def asRawPair: Option[RawPair[A]] =
    this match {
      case Pair(n1: Number[a], n2: Number[b], p) => Some(RawPair(n1, n2, p))
      case _                                     => None
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

// case class WithPath[A](content: A, path: Chain[Direction])
case class Number[A](n: A, path: Chain[Direction]) extends Tree[A] {
  def withPathPrefix(prefix: Chain[Direction]): Tree[A] = copy(path = prefix ++ path)
}

case class Pair[A](lhs: Tree[A], rhs: Tree[A], path: Chain[Direction]) extends Tree[A] {
  def withPathPrefix(prefix: Chain[Direction]): Tree[A] = copy(path = prefix ++ path)

  def select(dir: Direction): Tree[A] =
    dir match {
      case Left  => lhs
      case Right => rhs
    }

  def insertAt(dir: Direction, value: Tree[A]): Tree[A] = {
    val newChild = value.withPathPrefix(path.append(dir))
    dir match {
      case Left  => copy(lhs = newChild)
      case Right => copy(rhs = newChild)
    }
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
      char.flatMap(parseInt),
      path.pure[F],
    ).mapN(Number.apply)

  val pair: F[Snailfish] =
    (
      const('[') *> parseFull(path.append(Left)) <* const(','),
      parseFull(path.append(Right)) <* const(']'),
      path.pure[F],
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
    self.asRawPair.filter(_.path.size >= 4)
  }.get

def findNext(root: Snailfish, self: Snailfish, direction: Direction): Option[Snailfish] =
  // we wanna go left - drop everything including the first Right (from the end), reverse again and add Left
  self.path.toList.reverse.dropWhile(_ == direction) match {
    case _ :: t =>
      val newPath = Chain
        .fromSeq(t.reverse)
        .append(direction)
      root.getAt(newPath).some

    case _ => None
  }

it

it.left.n
it.right.n
it.path.toList

val nextBefore =
  findNext(root, it.asTree, Left)
    .map(_.collectLast(_.asLeaf).get)
    .get

val afterLeftReplace = root
  .replaceAt(nextBefore.path, Number(nextBefore.n + it.left.n, Chain.nil))

val nextAfter =
  findNext(root, it.asTree, Right)
    .map(_.collectFirst(_.asLeaf).get)
    .get

val afterRightReplace = afterLeftReplace
  .replaceAt(nextAfter.path, Number(nextAfter.n + it.right.n, Chain.nil))

val afterAllReplacements = afterRightReplace
  .replaceAt(it.path, Number(0, Chain.nil))

root.render
afterRightReplace.render
afterAllReplacements.render
