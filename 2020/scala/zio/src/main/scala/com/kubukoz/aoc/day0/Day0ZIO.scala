package com.kubukoz.aoc.day0

import com.kubukoz.aoc.ZUtil
import com.kubukoz.aoc.ZUtilApp
import zio.ZIO
import zio.console._

object Day0ZIO extends ZUtilApp {

  def runProg: ZIO[ZUtil with zio.ZEnv, Any, Any] =
    ZUtil
      .readFile("files/day0.txt")
      .flatMap(ZIO.foreach(_)(putStrLn(_)))

}
