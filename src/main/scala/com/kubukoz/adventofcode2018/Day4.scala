package com.kubukoz.adventofcode2018
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.{Duration, LocalDateTime, LocalTime}

import cats.data.NonEmptyList
import cats.effect.{Console, ExitCode, IO, IOApp, Sync}
import cats.implicits._
import cats.kernel.Order
import cats.{Monoid, data}
import fs2.{Chunk, Pipe, Pure, Stream}
import io.chrisdavenport.cats.time._
import monocle.macros.Lenses
import scalaz.deriving

object day4data {

  // @deriving(Order)
  case class GuardId(value: Int) extends AnyVal
  object GuardId {
    implicit val order: Order[GuardId] = Order.by(_.value)
  }

  sealed trait EventKind extends Product with Serializable
  case class BeginsShift(guardId: GuardId) extends EventKind
  case object FallsAsleep extends EventKind
  case object WakesUp extends EventKind

  case class Event(time: LocalDateTime, kind: EventKind)

  object Event {
    implicit val order: Order[Event] = Order.by(_.time)
  }

  @Lenses
  case class GuardHistory(guardId: GuardId, events: NonEmptyList[Event]) {
    def combineWith(another: GuardHistory): GuardHistory =
      copy(events = events ::: another.events)
  }

  //todo remove when https://github.com/ChristopherDavenport/cats-time/pull/42 is released
  implicit val durationMonoid: Monoid[Duration] = new Monoid[Duration] {
    override def empty: Duration = Duration.ZERO
    override def combine(x: Duration, y: Duration): Duration = x.plus(y)
  }

  case class GuardSleeps(values: List[GuardSleep]) extends AnyVal {
    def allTimes: Stream[Pure, LocalTime] =
      Stream
        .emits(values)
        .flatMap(_.times)

    def totalLength: Duration = values.foldMap(_.length)
  }

  object GuardSleeps {
    val fromEvents: NonEmptyList[Event] => GuardSleeps = { events =>
      val sleeps = Stream
        .emits(events.toList)
        .filter(!_.kind.isInstanceOf[BeginsShift])
        .chunkN(n = 2, allowFewer = false)
        .map(GuardSleep.fromChunk)

      GuardSleeps(sleeps.toList)
    }

    implicit val order: Order[GuardSleeps] = Order.by(_.totalLength)
  }

  case class GuardSleep(start: LocalDateTime, end: LocalDateTime) {
    def times: Stream[Pure, LocalTime] = {
      Stream
        .iterate(start)(_.plusMinutes(1L))
        .takeWhile(_.isBefore(end))
        .map(_.toLocalTime)
    }

    def length: Duration = Duration.between(start, end)
  }

  object GuardSleep {
    def fromChunk(chunk: Chunk[Event]): GuardSleep = {
      require(chunk.size == 2)
      val head = chunk.apply(0)
      val second = chunk.apply(1)

      GuardSleep(head.time, second.time)
    }
  }
}

object Day4 extends IOApp {
  import atto._
  import Atto._
  import com.kubukoz.adventofcode2018.day4data._

  val parser: Parser[Event] = {
    val fmt = new DateTimeFormatterBuilder()
      .append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral(' ')
      .append(DateTimeFormatter.ISO_LOCAL_TIME)
      .toFormatter

    val date = squareBrackets(manyN(16, anyChar))
      .map(_.mkString)
      .map(LocalDateTime.parse(_, fmt))

    val event = {
      val begins =
        (string("Guard #") *> int.map(GuardId(_)) <* string(" begins shift"))
          .map(BeginsShift)
          .widen[EventKind]

      val sleeps = string("wakes up").as(WakesUp).widen[EventKind]
      val falls = string("falls asleep").as(FallsAsleep).widen[EventKind]

      begins | sleeps | falls
    }

    (date <* spaceChar, event).mapN(Event.apply)
  }

  def guardHistories(events: List[Event]): List[GuardHistory] = {
    events
      .foldLeft(List.empty[GuardHistory]) {
        case (histories, ev @ Event(_, BeginsShift(newGuard))) =>
          GuardHistory(newGuard, data.NonEmptyList.one(ev)) :: histories

        case (history :: rest, event) =>
          GuardHistory.events.modify(event :: _)(history) :: rest

        case (Nil, _) => throw new Exception("impossible")
      }
      .groupByNel(_.guardId)
      .values
      .toList
      .map(_.reduceLeft(_ combineWith _))
      .map(GuardHistory.events.modify(_.sorted))
  }

  //there's probably an easier way
  def maxBy[F[_]: Sync, A, B](
    f: A => B
  )(implicit O: Order[B]): Pipe[F, A, A] = { stream =>
    stream
      .map(a => (a, f(a)))
      .reduce[(A, B)] {
        case (prev @ (_, maxB), that @ (_, b)) =>
          if (maxB >= b) prev
          else that
      }
      .map(_._1)
  }

  def mostFrequentMinute(times: List[LocalTime]): Option[(Int, Int)] = {
    times
      .groupByNel(_.getMinute)
      .mapValues(_.size)
      .toList
      .maximumOption(Order.by(_._2))
  }

  def part1[F[_]: Sync](events: List[Event]): F[Option[Int]] = {

    guardStats(events)
      .through(maxBy(_._2))
      .map {
        case (guard, sleeps) =>
          mostFrequentMinute(sleeps.allTimes.toList).map {
            case (minute, _) =>
              guard.value * minute
          }
      }
      .unNone
      .compile
      .last
  }

  def part2[F[_]: Sync](events: List[Event]): F[Option[Int]] = {
    guardStats(events)
      .map {
        case (guard, sleeps) =>
          mostFrequentMinute(sleeps.allTimes.toList).map {
            case (minute, freq) =>
              (guard, minute, freq)
          }
      }
      .unNone
      .through(maxBy(_._3))
      .map {
        case (guard, minute, _) =>
          guard.value * minute
      }
      .compile
      .last
  }

  private def guardStats(
    events: List[Event]
  ): Stream[Pure, (GuardId, GuardSleeps)] = {
    Stream
      .emits(guardHistories(events))
      .map(hist => hist.guardId -> GuardSleeps.fromEvents(hist.events))
  }
  override def run(args: List[String]): IO[ExitCode] = {

    implicit val console = Console.io
    import atto.fs2.Pipes._
    import console._

    val elems = fileLines[IO]("/day4.txt")
      .through(parseN[IO, Event](parser))
      .compile
      .toList
      .map(_.sortBy(_.time))

    elems
      .flatMap { events =>
        (part1[IO](events), part2[IO](events))
          .parMapN(putStrLn(_) *> putStrLn(_))
          .flatten
      }
      .as(ExitCode.Success)
  }
}
