package com.kubukoz.aoc.day0

import zio.ExitCode
import zio.console._
import com.kubukoz.aoc.ZUtil

object Day0ZIO extends zio.App {

  def run(args: List[String]): zio.URIO[zio.ZEnv, ExitCode] =
    ZUtil
      .readFile("files/day0.txt")
      .flatMap(putStrLn(_))
      .exitCode

}
