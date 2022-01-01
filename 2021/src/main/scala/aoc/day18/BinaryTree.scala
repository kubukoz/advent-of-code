package aoc.day18

import cats.data.Chain
import cats.implicits._
import cats.kernel.Semigroup
import monocle.Lens
import monocle.Optional

sealed trait BinaryTree[A] {

  def render: String = fold(_.toString)((l, r) => s"[$l,$r]")

  def down(dir: Direction): BinaryTree[A] = BinaryTree.down(dir).get(this)

  def getAt(path: Chain[Direction]): BinaryTree[A] = BinaryTree.at(path).get(this)

  def replaceAt(
    path: Chain[Direction],
    newValue: BinaryTree[A],
  ): BinaryTree[A] = BinaryTree.at(path).replace(newValue)(this)

  def collectFirst[B](
    withNode: WithPath[BinaryTree[A]] => Option[B],
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
    withNode: WithPath[BinaryTree[A]] => Option[B],
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

  def asLeaf: Option[Leaf[A]] =
    this match {
      case n @ Leaf(_) => Some(n)
      case _           => None
    }

  def asPair: Option[Pair[A]] =
    this match {
      case p @ Pair(_, _) => Some(p)
      case _              => None
    }

  def asRawPair: Option[RawPair[A]] =
    this match {
      case Pair(n1: Leaf[a], n2: Leaf[b]) => Some(RawPair(n1.n, n2.n))
      case _                              => None
    }

  def magnitude(implicit A: Semigroup[A]): A = fold(identity)(_.combineN(3) |+| _.combineN(2))

  def fold[B](number: A => B)(pair: (B, B) => B): B =
    this match {
      case Leaf(n)        => number(n)
      case Pair(lhs, rhs) => pair(lhs.fold(number)(pair), rhs.fold(number)(pair))
    }

}

object BinaryTree {

  def next[A](
    self: WithPath[BinaryTree[A]],
    direction: Direction,
  ): Optional[BinaryTree[A], WithPath[A]] = {

    // we wanna go left - drop everything including the first Right (from the end), reverse again and add Left
    val newPathPrefix =
      self.path.toList.reverse.dropWhile(_ == direction) match {
        case _ :: t => Chain.fromSeq(t.reverse).append(direction).some
        case _      => None
      }

    Optional[BinaryTree[A], WithPath[A]] { root =>
      newPathPrefix.flatMap { case newPath =>
        WithPath(newPath, root.getAt(newPath))
          .flatTraverse { nextBranch =>
            direction match {
              case Left  => nextBranch.collectLast(_.traverse(_.asLeaf))
              case Right => nextBranch.collectFirst(_.traverse(_.asLeaf))
            }
          }
          .map(_.map(_.n))
      }
    }(next => _.replaceAt(next.path, Leaf(next.content)))
  }

  def down[A](direction: Direction): Lens[BinaryTree[A], BinaryTree[A]] =
    Lens[BinaryTree[A], BinaryTree[A]] {
      case Leaf(n)        => sys.error("can't go down on number")
      case p @ Pair(_, _) => p.select(direction)
    } { newValue =>
      {
        case Leaf(n)        => sys.error("can't go down on number")
        case p @ Pair(_, _) => p.insertAt(direction, newValue)
      }
    }

  def at[A](
    path: Chain[Direction]
  ): Lens[BinaryTree[A], BinaryTree[A]] = path.map(down[A]).toList.reduceLeft(_ andThen _)

}

case class Leaf[A](n: A) extends BinaryTree[A]

case class Pair[A](lhs: BinaryTree[A], rhs: BinaryTree[A]) extends BinaryTree[A] {

  def select(dir: Direction): BinaryTree[A] =
    dir match {
      case Left  => lhs
      case Right => rhs
    }

  def insertAt(dir: Direction, value: BinaryTree[A]): BinaryTree[A] =
    dir match {
      case Left  => copy(lhs = value)
      case Right => copy(rhs = value)
    }

}

case class RawPair[A](left: A, right: A) {
  def asTree: BinaryTree[A] = Pair(Leaf(left), Leaf(right))

  def select(dir: Direction): A =
    dir match {
      case Left  => left
      case Right => right
    }

}
