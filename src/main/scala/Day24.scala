import Combinator.CombinationRequest
import Day24.Combinations
import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern._
import akka.routing.RoundRobinPool
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Day24 {
  type Combinations = List[Set[Int]]
  def main(args: Array[String]) {
    val input =
      """1
        |3
        |5
        |11
        |13
        |17
        |19
        |23
        |29
        |31
        |41
        |43
        |47
        |53
        |59
        |61
        |67
        |71
        |73
        |79
        |83
        |89
        |97
        |101
        |103
        |107
        |109
        |113""".stripMargin.split("\n").map(_.toInt)

    val system = ActorSystem()
    implicit val timeout = Timeout(2.hours)
    implicit val dispatcher = system.dispatcher
    val combinator = system.actorOf(Props[Combinator].withRouter(RoundRobinPool(100)))

    def quantumEntanglement(group: Traversable[Int]) = group.map(_.toLong).product
    val start = System.currentTimeMillis
    val sumAll = input.sum // 1548
    val thirdSum = sumAll / 3
    val allCombs = Await.result(
      Future.sequence((1 to input.length)
        .map(combinator ? CombinationRequest(input, _, thirdSum))
        .map(_.mapTo[Combinations])),
      Duration.Inf).flatten

    system.terminate()

    println("All combs: " + allCombs.size)
    val triples = allCombs.combinations(3).filter { comb => (comb.head & comb(1) & comb.last).isEmpty}
      //turns out that the first combination that gets up to this point is, indeed, the valid one
      .map(_.toList).toList
    println("Triples: " + triples.size)
    val smallestTripleSize = triples.map(_.map(_.size).min).min
    println(s"Smallest triple has $smallestTripleSize elements")
    println(triples.filter(_.exists(_.size == smallestTripleSize)).map(tr => quantumEntanglement(tr.minBy(_.size))).min)
    println(s"Found in ${(System.currentTimeMillis - start) / 60000} minutes")
  }
}

class Combinator extends Actor {
  implicit val dispatcher = context.dispatcher

  override def receive: Receive = {
    case CombinationRequest(input, size, expectedSize) => Future {
      input.combinations(size).filter(_.sum == expectedSize).map(_.toSet).toList
    } pipeTo sender
  }
}

object Combinator {
  case class CombinationRequest(input: Array[Int], size: Int, expectedSize: Int)
}
