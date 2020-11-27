package com.kubukoz.aoc

import zio.ExitCode
import zio.ZIO

abstract class ZUtilApp extends zio.App {
  def run(args: List[String]): zio.URIO[zio.ZEnv, ExitCode] = runProg.provideCustomLayer(ZUtil.layer).exitCode

  def runProg: ZIO[ZUtil with zio.ZEnv, Any, Any]
}
