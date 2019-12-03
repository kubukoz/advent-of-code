import Reindeer.{GetPoints, GivePoint, GetDist, NextSecond}
import akka.actor.{ActorRef, ActorSystem, Props, Actor}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Day14 {
  val pattern = "([A-z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds\\.".r
  val input =
    """Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
      |Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
      |Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
      |Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
      |Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
      |Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
      |Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
      |Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
      |Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds.""".stripMargin.split("\n").map {
      case pattern(name, speed, length, rest) => ReindeerInfo(name, speed.toInt, length.toInt, rest.toInt)
    }.toList

  def main(args: Array[String]) {
    val system = ActorSystem("reindeers")
    val defaultTimeout = 10.seconds
    implicit val timeout = Timeout(defaultTimeout)
    implicit val dispatcher = system.dispatcher

    def getDistances(reindeers: List[ActorRef]) = Future.sequence(reindeers.map(actor => (actor ? GetDist).mapTo[(ActorRef, Int)]))
    def getPoints(reindeers: List[ActorRef]) = Future.sequence(reindeers.map(actor => (actor ? GetPoints).mapTo[Int]))

    val reindeers = input.map(Reindeer.props).map(system.actorOf)

    val endTime = 2503
    var time = 0

    while (time < endTime) {
      reindeers.foreach(_ ! NextSecond)
      time += 1
      Await.result(getDistances(reindeers), defaultTimeout).maxBy(_._2)._1 ! GivePoint
    }

    (getDistances(reindeers) zip getPoints(reindeers)).foreach { case (distances, points) =>
      println("Max distance: " + distances.map(_._2).max)
      println("Max points: " + points.max)
      system.terminate()
    }
  }
}

case class ReindeerInfo(name: String, speed: Int, length: Int, rest: Int)

class Reindeer(val speed: Int, val maxRun: Int, val maxRest: Int) extends Actor {
  var resting = false
  var distance = 0
  var remainingRun = maxRun
  var remainingRest = 0
  var points = 0

  override def receive: Receive = {
    case NextSecond if resting =>
      remainingRest -= 1
      if (remainingRest == 0) {
        remainingRun = maxRun
        resting = false
      }
    case NextSecond if !resting =>
      remainingRun -= 1
      distance += speed
      if (remainingRun == 0) {
        remainingRest = maxRest
        resting = true
      }
    case GetDist => sender !(self, distance)
    case GivePoint => points += 1
    case GetPoints => sender ! points
  }
}

object Reindeer {
  def props(reindeerInfo: ReindeerInfo) = Props(creator = new Reindeer(reindeerInfo.speed, reindeerInfo.length, reindeerInfo.rest))

  case object NextSecond

  case object GivePoint

  case object GetDist

  case object GetPoints

}
