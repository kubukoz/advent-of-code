import cats.effect.IO
import com.kubukoz.aoc.Util
import cats.effect.unsafe.implicits._

Util
  .readFile[IO]("files/day1.txt")
  .unsafeRunSync()
  .map(_.trim)
  .filter(_.nonEmpty)
  .map(_.toInt)
  .combinations(3)
  .find { grp =>
    grp.sum == 2020
  }
  .map(_.product)
