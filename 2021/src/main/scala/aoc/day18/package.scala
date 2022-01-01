package aoc

import cats.data.Chain
import cats.data.Writer

package object day18 {

  type WithPath[A] = Writer[Chain[Direction], A]
  val WithPath = Writer

  implicit class WithPathOps[A](wp: WithPath[A]) {
    def content: A = wp.value
    def path: Chain[Direction] = wp.written
  }

  type Snailfish = BinaryTree[Int]

}
