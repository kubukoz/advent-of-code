package com.kubukoz.adventofcode2017

import scala.annotation.tailrec

object Day7 {

  final case class ParsedNode(name: String, weight: Int, childNames: List[String]) {
    def toEmptyNode: Tree = Tree(name, Nil, weight)
  }

  final case class Tree(name: String, children: List[Tree], weight: Int) {
    val allNodes: Stream[String] = name #:: children.toStream.flatMap(_.allNodes)

    def addChild(to: String, child: Tree): Tree = {
      val newChildren = if (name == to) {
        child :: children
      }
      else children.map(_.addChild(to, child))

      copy(children = newChildren)
    }

    def sum: Int = weight + children.map(_.sum).sum

    def isUnbalanced: Boolean = {
      children.map(_.sum).toSet.size > 1
    }

    def balanceOffset: Option[Int] = {
      val childrenBySum = children.groupBy(_.sum)
      val areSumsEqual = childrenBySum.size <= 1
      if (areSumsEqual) None
      else for {
        (wrongSum, offendingChild) <- childrenBySum.collectFirst { case (s, ch :: Nil) => (s, ch) }
        validSize <- childrenBySum.collectFirst { case (s, _) if s != wrongSum => s }

        result <- {
          if (offendingChild.isUnbalanced) offendingChild.balanceOffset
          else Some(validSize - (offendingChild.sum - offendingChild.weight))
        }
      } yield result
    }
  }

  private val pat = """(\w+) \((\d+)\)""".r
  private val pat2 = """(\w+) \((\d+)\) -> (.+)""".r

  val parse: String => ParsedNode = {
    case pat(name, weight) =>
      ParsedNode(name, weight.toInt, Nil)
    case pat2(name, weight, childList) =>
      ParsedNode(name, weight.toInt, childList.split(",").map(_.trim).toList)
  }

  def buildTree(parsed: List[ParsedNode]): Option[Tree] = {
    val parsedByName = parsed.groupBy(_.name).mapValues(_.head)

    val childNames = parsed.flatMap(_.childNames).toSet

    val treeRoot = parsed.find { root =>
      !childNames(root.name)
    }.map(_.toEmptyNode)

    @tailrec
    def go(remaining: Set[String], current: Tree): Tree = {
      val nextToInsert = remaining.view.flatMap { toInsert =>
        current.allNodes.find {
          parsedByName(_).childNames.contains(toInsert)
        }.map(toInsert -> _)
      }

      nextToInsert.headOption match {
        case None => current
        case Some((next, addTo)) =>
          val treeToAdd = parsedByName(next).toEmptyNode
          go(remaining - next, current.addChild(addTo, treeToAdd))
      }
    }

    treeRoot.map(go(childNames, _))
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day7.txt")
    val parsed = input.map(parse)

    val tree = buildTree(parsed).get
    println(tree.name)
    println(tree.balanceOffset)
  }
}
