package com.kubukoz.adventofcode2018

import atto.Atto._
import atto.Parser
import cats.effect.concurrent.Ref
import cats.effect._
import cats.implicits._
import cats.mtl.MonadState
import cats.temp.par._
import fs2.Stream
import monocle.macros.Lenses

object Day3 extends IOApp {

  case class ClaimId(value: Int) extends AnyVal

  case class Claim(id: ClaimId, left: Int, top: Int, width: Int, height: Int) {
    def area: Int = width * height
  }

  val parser: Parser[Claim] = (
    char('#') *> int.map(ClaimId) <* string(" @ "),
    int <* char(','),
    int <* string(": "),
    int <* char('x'),
    int
  ).mapN(Claim)

  @Lenses
  case class ClaimedBox(value: List[ClaimId]) extends AnyVal

  @Lenses
  case class ClaimState(value: List[List[ClaimedBox]]) extends AnyVal

  object ClaimState {
    type MState[F[_]] = MonadState[F, ClaimState]
    def MState[F[_]](implicit F: MState[F]): MState[F] = F
  }

  private def useClaim[F[_]: ClaimState.MState](claim: Claim): F[Unit] =
    (ClaimState.MState[F].modify _)
      .compose(ClaimState.value.modify)
      .compose(modifyBetween(claim.top, claim.height))
      .compose(modifyBetween(claim.left, claim.width))
      .apply(ClaimedBox.value.modify(claim.id :: _))

  private def singleClaimedAreas(state: ClaimState): Map[ClaimId, Int] = {
    state.value.foldMap {
      _.foldMap {
        case ClaimedBox(List(id)) => Map(id -> 1)
        case _                    => Map.empty[ClaimId, Int]
      }
    }
  }

  def solution[F[_]: ClaimState.MState: Sync: Par: Clock: ConsoleOut](
    lines: Stream[F, String]
  ): F[(Int, Option[ClaimId])] = {
    val parsed: Stream[F, Claim] = lines.map { line =>
      parser
        .parseOnly(line)
        .option
        .getOrElse(throw new Exception(s"parsing failed in line $line"))
    }

    val getPart1 =
      ClaimState
        .MState[F]
        .get
        .map(_.value.flatten.map(_.value.size).count(_ >= 2))

    val getPart2 =
      ClaimState
        .MState[F]
        .get
        .map(singleClaimedAreas)
        .flatMap { areas =>
          val isFullOnes: Claim => Boolean =
            claim => areas.get(claim.id).contains(claim.area)

          parsed.filter(isFullOnes).map(_.id).compile.last
        }

    parsed.evalMap(useClaim[F]).compile.drain *> (getPart1, getPart2).tupled
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val console: Console[IO] = cats.effect.Console.io
    import console._

    val lines = fileLines[IO]("/day3.txt").filter(_.nonEmpty)

    import com.olegpy.meow.effects._

    Ref[IO]
      .of(ClaimState(List.fill(1000)(List.fill(1000)(ClaimedBox(Nil)))))
      .flatMap {
        _.runState { implicit F =>
          measure("solution")(solution(lines))
        }
      }
      .map(_.toString)
      .flatMap(putStrLn(_))
  }.as(ExitCode.Success)
}
