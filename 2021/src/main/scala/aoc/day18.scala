package aoc

import aoc.lib._
import cats.Defer
import cats.Eval
import cats.MonadError
import cats.data.Chain
import cats.data.EitherT
import cats.data.StateT
import cats.data.Writer
import cats.implicits._
import cats.kernel.Semigroup
import cats.mtl.Stateful

object Day18 extends App {

  case class RawPair[A](left: Number[A], right: Number[A]) {
    def asTree: Tree[A] = Pair(left, right)

    def select(dir: Direction): Number[A] =
      dir match {
        case Left  => left
        case Right => right
      }

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
    ): Option[B] = withNode(WithPath(pathSoFar, this)).orElse {
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
    }.orElse(withNode(WithPath(pathSoFar, this)))

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

    def magnitude(implicit A: Semigroup[A]): A =
      fold(
        identity
      )(_.combineN(3) |+| _.combineN(2))

    def fold[B](number: A => B)(pair: (B, B) => B): B =
      this match {
        case Number(n)      => number(n)
        case Pair(lhs, rhs) => pair(lhs.fold(number)(pair), rhs.fold(number)(pair))
      }

  }

  sealed trait Direction

  case object Left extends Direction
  case object Right extends Direction

  type WithPath[A] = Writer[Chain[Direction], A]
  val WithPath = Writer

  implicit class WithPathOps[A](wp: WithPath[A]) {
    def content: A = wp.value
    def path: Chain[Direction] = wp.written
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

    val number: F[Snailfish] =
      (
        char.flatMap(parseInt)
      ).map(Number.apply)

    val pair: F[Snailfish] =
      (
        const('[') *> self <* const(','),
        self <* const(']'),
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
    parseFull[StateT[EitherT[Eval, Throwable, *], (String, Int), *]]
      .runA((s, 0))
      .value
      .value
      .toTry
      .get

  def findNext(
    root: Snailfish,
    self: WithPath[Snailfish],
    direction: Direction,
  ): Option[WithPath[Number[Int]]] =
    // we wanna go left - drop everything including the first Right (from the end), reverse again and add Left
    self.path.toList.reverse.dropWhile(_ == direction) match {
      case _ :: t =>
        val newPath = Chain
          .fromSeq(t.reverse)
          .append(direction)

        WithPath(newPath, root.getAt(newPath)).flatTraverse { nextBranch =>
          direction match {
            case Left  => nextBranch.collectLast(_.traverse(_.asLeaf))
            case Right => nextBranch.collectFirst(_.traverse(_.asLeaf))
          }
        }
      case _ => None
    }

  def replaceNextIn(it: WithPath[RawPair[Int]], direction: Direction): Snailfish => Snailfish =
    root =>
      findNext(root, it.map(_.asTree), direction)
        .fold(root) { neighbor =>
          root
            .replaceAt(neighbor.path, Number(neighbor.content.n + it.content.select(direction).n))
        }

  def rawExplode(
    it: WithPath[RawPair[Int]]
  ): Snailfish => Snailfish = _.replaceAt(it.path, Number(0))

  def tryExplode(root: Snailfish): Option[Snailfish] = root
    .collectFirst { self =>
      self.traverse(_.asRawPair.filter(_ => self.path.size >= 4))
    }
    .map { it =>
      replaceNextIn(it, Left)
        .andThen(replaceNextIn(it, Right))
        .andThen(rawExplode(it))
        .apply(root)
    }

  def trySplit(root: Snailfish): Option[Snailfish] = root
    .collectFirst { self =>
      self.traverse(_.asLeaf).filter(_.content.n >= 10)
    }
    .map { it =>
      val n = it.content.n
      val low = n / 2
      val high = (n / 2.0f).round

      root.replaceAt(it.path, Pair(Number(low), Number(high)))
    }

  def reduce(
    root: Snailfish
  ): Snailfish = tryExplode(root)
    .orElse(trySplit(root))
    .map(reduce)
    .getOrElse(root)

  reduce(parsePair("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")).render

  val fromFile = readAllLines("day18.txt")

  val example =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".split("\n").toList

  val input = fromFile

// val input = example

  val data = input
    .map(parsePair)

  data.reduceLeft((a, b) => reduce(Pair(a, b))).render
  data.reduceLeft((a, b) => reduce(Pair(a, b))).magnitude

  (data, data)
    .tupled
    .filterNot { case (a, b) => a == b }
    .map { case (a, b) => reduce(Pair(a, b)).magnitude }
    .max
}
