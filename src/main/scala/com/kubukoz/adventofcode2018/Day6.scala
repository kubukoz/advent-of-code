package com.kubukoz.adventofcode2018
import cats.data.{NonEmptyList, NonEmptySet, Reader, StateT}
import cats.{Applicative, FlatMap, Order, Show}
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl.{ApplicativeAsk, DefaultApplicativeAsk, MonadState}
import fs2.Pure
import monocle.{Lens, Prism}
import monocle.macros.{GenLens, GenPrism, Lenses}
import scalaz.deriving

import scala.collection.immutable.SortedSet

object day6Data {
  @deriving(Order, Show)
  case class Coordinate(x: Int, y: Int)

  object Coordinate {
    val x: Lens[Coordinate, Int] = GenLens[Coordinate](_.x)
    val y: Lens[Coordinate, Int] = GenLens[Coordinate](_.y)

    def distance(a: Coordinate, b: Coordinate): Int = {
      (a.x - b.x).abs + (a.y - b.y).abs
    }
  }

  case class Coordinates(value: NonEmptySet[Coordinate]) extends AnyVal

  object Coordinates {
    type Ask[F[_]] = ApplicativeAsk[F, Coordinates]
    def Ask[F[_]](implicit F: Ask[F]): Ask[F] = F
  }

  case class ResultState(byOwner: Map[Coordinate, Set[Coordinate]])

  object ResultState {
    def fromMarkers(markers: Map[Coordinate, UsageStatus]): ResultState =
      ResultState {
        markers.toList
          .groupBy {
            case (_, v) =>
              UsageStatus.usedOnce.getOption(v).map(_.owner)
          }
          .collect {
            case (Some(k), v) => k -> v.map(_._1).toSet
          }
      }
  }

  sealed trait UsageStatus extends Product with Serializable
  object UsageStatus {
    case class UsedOnce(owner: Coordinate) extends UsageStatus
    case object UsedMoreThanOnce extends UsageStatus

    val usedOnce: Prism[UsageStatus, UsedOnce] = GenPrism[UsageStatus, UsedOnce]
  }

  @Lenses
  case class Trip(markers: Map[Coordinate, UsageStatus],
                  latestRoots: Set[Coordinate],
                  round: Int)

  object Trip {
    type State[F[_]] = MonadState[F, Trip]
    def State[F[_]](implicit F: State[F]): State[F] = F

    def initialState(coords: Set[Coordinate]): Trip =
      Trip(Map.empty, coords, 1)
  }
}

object Day6 extends IOApp {
  val pat = """(\d+), (\d+)""".r
  import day6Data._

  import com.olegpy.meow.effects._

  def part1[F[_]: Sync: ContextShift: ConsoleOut]: F[Int] = {
    fileLines("/day6.txt")
      .filter(_.nonEmpty)
      .map {
        case pat(x, y) => Coordinate(x.toInt, y.toInt)
      }
      .compile
      .toList
      .map(_.to[SortedSet])
      .flatMap { coords =>
        val maxDistance = getMaxDistance(coords)

        implicit val readCoords: Coordinates.Ask[F] =
          new DefaultApplicativeAsk[F, Coordinates] {
            override val applicative: Applicative[F] = implicitly
            override val ask: F[Coordinates] =
              Coordinates(NonEmptySet.fromSetUnsafe(coords)).pure[F]
          }

        Ref[F].of(Trip.initialState(coords)).flatMap { ref =>
          implicit val S = ref.stateInstance

          for {
            _ <- (1 to maxDistance).toList.traverse_[F, Unit](_ => makeStep[F])
            stateAtMaxDistance <- ref.get
            _ <- makeStep[F]
            nextState <- ref.get
          } yield {
            val (state1, state2) =
              (
                ResultState.fromMarkers(stateAtMaxDistance.markers),
                ResultState.fromMarkers(nextState.markers)
              )
            val finiteCoords = coords.filter(sameState(state1, state2))

            finiteCoords.map(state1.byOwner.mapValues(_.size)).max + 1
          }
        }
      }

  }

  private def getMaxDistance(coords: SortedSet[Coordinate]): Int = {
    coords
      .subsets(2)
      .map { set =>
        val a = set.head
        val b = set.tail.head
        Coordinate.distance(a, b)
      }
      .max
  }

  private def sameState(state1: ResultState,
                        state2: ResultState): Coordinate => Boolean = {
    val zipped: Map[Coordinate, Boolean] =
      (state1.byOwner, state2.byOwner).mapN(_ == _)

    zipped.getOrElse(_, false)
  }

  private def makeStep[F[_]: Trip.State: Coordinates.Ask: FlatMap: ConsoleOut]
    : F[Unit] =
    for {
      coordinates <- Coordinates.Ask[F].ask
      before <- Trip.State.get

      newPoints = before.latestRoots.flatMap(moves) -- before.markers.keySet -- coordinates.value.toSortedSet

      newMarkers = newPoints.map(
        coord => (coord, getUsageStatus(coord, coordinates))
      )

      _ <- ConsoleOut[F].putStrLn(s"round ${before.round}")

      _ <- Trip.State.modify {
        Trip.latestRoots.set(newPoints) compose
          Trip.markers.modify(_ ++ newMarkers) compose
          Trip.round.modify(_ + 1)
      }
    } yield ()

  private def getUsageStatus(point: Coordinate,
                             coordinates: Coordinates): UsageStatus = {
    val distances = coordinates.value.groupBy(Coordinate.distance(point, _))

    val possibleOwners = distances.toSortedMap.minBy(_._1)._2

    if (possibleOwners.size == 1) UsageStatus.UsedOnce(possibleOwners.head)
    else UsageStatus.UsedMoreThanOnce
  }

  private def moves(coordinate: Coordinate): Set[Coordinate] = {
    import Coordinate.{x, y}
    val deltas = (-1 to 1).toList.map(d => (_: Int) + d)

    (deltas.map(x.modify), deltas.map(y.modify))
      .mapN(_ compose _)
      .map(_(coordinate))
      .toSet
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val console = Console.io
    import console._

    part1[IO].flatMap(putStrLn(_)).as(ExitCode.Success)
  }
}
