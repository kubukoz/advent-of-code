package com.kubukoz.adventofcode2017
import fastparse.all._

object Day9 {

  sealed trait Thing extends Product with Serializable

  final case class Group(things: List[Thing]) extends Thing {
    def score: Int = scoreMem(0)

    private def scoreMem(mem: Int): Int = {
      val thisScore = mem + 1
      thisScore + groups.map(_.scoreMem(thisScore)).sum
    }

    def garbages: List[Garbage] = things.collect {
      case g: Garbage => g :: Nil
      case g: Group => g.garbages
    }.flatten

    private def groups: List[Group] = things.collect { case g: Group => g }
  }

  final case class Garbage(value: String) extends Thing


  private val group: P[Group] = P("{" ~ thing.rep(sep = ",") ~ "}").map(things => Group(things.toList))
  private val garbage: P[Garbage] = P("<" ~ (("!" ~ AnyChar).!.map(_ => "") | CharPred(_ != '>').!).rep ~ ">").map(x => Garbage(x.mkString))

  private val thing: P[Thing] = group | garbage

  def parse(s: String): Thing = thing.parse(s).get.value

  def score(s: String): Int = group.parse(s).get.value.score

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day9.txt").mkString("\n")
    val parsed = group.parse(input).get.value

    println(parsed.score)
    println(parsed.garbages.map(_.value.length).sum)
  }

}
