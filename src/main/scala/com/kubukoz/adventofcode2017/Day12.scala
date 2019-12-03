package com.kubukoz.adventofcode2017

object Day12 {

  final case class Connection(left: Int, right: Int) {
    def asFrom(from: Int): Option[Connection] = from match {
      case `left` => Some(this)
      case `right` => Some(Connection(right, left))
      case _ => None
    }
  }

  private def parse(line: String): List[Connection] = {
    line.split("""\s*<->\s*""") match {
      case Array(left, rights) =>
        rights.split("""\s*,\s*""").map(_.toInt).map(Connection(left.toInt, _))
    }
  }.toList

  private def groupContaining(connections: List[Connection], target: Int): Set[Int] = {
    def go(from: Int, seen: Set[Int]): Set[Int] = {
      val directFromTarget = connections.flatMap(_.asFrom(from)).map(_.right).toSet -- seen
      directFromTarget ++ directFromTarget.flatMap(go(_, seen ++ directFromTarget))
    }

    go(target, Set.empty)
  }

  def groupsIn(connections: List[Connection]): Set[Set[Int]] = {
    def go(elemsLeft: Set[Int], mem: Set[Set[Int]]): Set[Set[Int]] = elemsLeft.headOption match {
      case Some(h) =>
        val hGroup = groupContaining(connections, h)

        go(elemsLeft -- hGroup, mem + hGroup)

      case _ => mem
    }

    go(connections.map(_.left).toSet, Set.empty)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day12.txt").flatMap(parse)
    val allGroups = groupsIn(input)

    println(allGroups.find(_.contains(0)).get.size)
    println(allGroups.size)
  }
}
