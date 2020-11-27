package com.kubukoz.aoc

import zio.stream.Stream
import java.nio.file.Paths
import zio.stream.ZTransducer
import zio.blocking.Blocking
import zio.IO
import zio.ZLayer
import zio.macros.accessible

@accessible
object ZUtil {

  trait Service {
    def readFile(name: String): IO[Throwable, String]
  }

  val layer: ZLayer[Blocking, Nothing, ZUtil] = ZLayer.fromFunction[Blocking, Service] { blocking =>
    new Service {
      def readFile(name: String): zio.IO[Throwable, String] =
        Stream
          .fromFile(Paths.get(name))
          .transduce(ZTransducer.utf8Decode)
          .runCollect
          .map(_.mkString)
          .provide(blocking)
    }
  }

}
