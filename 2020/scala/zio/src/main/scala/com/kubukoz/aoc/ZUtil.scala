package com.kubukoz.aoc

import zio.ZIO
import zio.stream.Stream
import java.nio.file.Paths
import zio.stream.ZTransducer
import zio.blocking.Blocking

object ZUtil {

  def readFile(name: String): ZIO[Blocking, Throwable, String] =
    Stream
      .fromFile(Paths.get(name))
      .transduce(ZTransducer.utf8Decode)
      .runCollect
      .map(_.mkString)

}
