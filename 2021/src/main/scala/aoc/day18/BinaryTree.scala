package aoc.day18

import cats.data.Chain
import cats.implicits._
import cats.kernel.Semigroup

sealed trait BinaryTree[A] {

  def render: String =
    this match {
      case Leaf(n)        => n.toString()
      case Pair(lhs, rhs) => s"[${lhs.render},${rhs.render}]"
    }

  def down(dir: Direction): BinaryTree[A] =
    this match {
      case Leaf(n)        => sys.error("can't go down on number")
      case p @ Pair(_, _) => p.select(dir)
    }

  def getAt(path: Chain[Direction]): BinaryTree[A] = path.toList.foldLeft(this)(_.down(_))

  def replaceAt(path: Chain[Direction], newValue: BinaryTree[A]): BinaryTree[A] = {
    def go(rest: List[Direction], self: BinaryTree[A]): BinaryTree[A] =
      rest match {
        case Nil      => sys.error("unsupported (replace whole tree)")
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

  def downReplace(dir: Direction, newValue: BinaryTree[A]): BinaryTree[A] =
    this match {
      case Leaf(n)        => sys.error("can't go down on number")
      case p @ Pair(_, _) => p.insertAt(dir, newValue)
    }

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

  def findNext(
    self: WithPath[BinaryTree[A]],
    direction: Direction,
  ): Option[WithPath[A]] =
    // we wanna go left - drop everything including the first Right (from the end), reverse again and add Left
    self.path.toList.reverse.dropWhile(_ == direction) match {
      case _ :: t =>
        val newPath = Chain
          .fromSeq(t.reverse)
          .append(direction)

        WithPath(newPath, this.getAt(newPath))
          .flatTraverse { nextBranch =>
            direction match {
              case Left  => nextBranch.collectLast(_.traverse(_.asLeaf))
              case Right => nextBranch.collectFirst(_.traverse(_.asLeaf))
            }
          }
          .map(_.map(_.n))
      case _ => None
    }

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
