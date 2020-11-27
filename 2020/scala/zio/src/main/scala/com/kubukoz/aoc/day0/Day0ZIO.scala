package com.kubukoz.aoc.day0

import com.kubukoz.aoc.ZUtil
import com.kubukoz.aoc.ZUtilApp
import zio.Has
import zio.ZIO
import zio.console._

object Day0ZIO extends ZUtilApp {

  def runProg: ZIO[Has[ZUtil.Service] with zio.ZEnv, Any, Any] =
    ZUtil
      .readFile("files/day0.txt")
      .flatMap(putStrLn(_))

}
