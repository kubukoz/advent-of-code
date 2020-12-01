package com.kubukoz.aoc.day1

import cats.effect.IOApp
import cats.effect.IO
import com.kubukoz.aoc.Util
import cats.implicits._

object Day1 extends IOApp.Simple {

  def run: IO[Unit] =
    Util
      .readFile[IO]("files/day1.txt")
      .map(_.map(_.trim).filter(_.nonEmpty).map(_.toInt))
      .flatMap { nums =>
        List(2, 3)
          .parTraverse { i =>
            IO {
              nums
                .combinations(i)
                .find(_.sum == 2020)
                .map(_.product)
            }
          }
          .flatMap(_.traverse(IO.println(_)))
      }
      .void

}
