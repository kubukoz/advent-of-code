package com.kubukoz.adventofcode2017

object Day24 {
  case class Component(left: Int, right: Int){
    def sum: Int = left + right
    def swap = Component(right, left)
  }

  private val pat = """(\d+)/(\d+)""".r
  private val parse: String => Component = {
    case pat(l, r) => Component(l.toInt, r.toInt)
  }

  def sum(components: List[Component]): Int = {
    components.map(_.sum).sum
  }

  def remove[T](i: Int, from: List[T]): List[T] = {
    val (left, _ :: right) = from.splitAt(i)
    left ::: right
  }

  def findCombinations(components: List[Component]): List[List[Component]] = {
    def go(remaining: List[Component], mem: List[List[Component]], lastElem: Component): List[List[Component]] = {
      if(remaining.isEmpty) mem
      else {
        val potentialNext = remaining.zipWithIndex.collect {
          case (h, i) if lastElem.right == h.left => (h, i)
          case (h, i) if lastElem.right == h.right => (h.swap, i)
        }

        if (potentialNext.isEmpty) mem else {
          potentialNext.flatMap { case (next, i) =>
            go(remove(i, remaining), mem.map(next :: _), next)
          }
        }
      }
    }

    val startMem = components.zipWithIndex.collect {
      case (s@Component(0, _), i) => (s, i)
      case (s@Component(_, 0), i) => (s.swap, i)
    }

    startMem.flatMap { case (init, i) =>
      go(remove(i, components), List(List(init)), init)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day24.txt").map(parse)

    val all = findCombinations(input)
    println(sum(all.maxBy(sum)))
    println(sum(all.maxBy(a => (a.size, sum(a)))))

  }
}
