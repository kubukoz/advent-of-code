package com.kubukoz.adventofcode2018
import java.time.LocalTime

import cats.Show
import cats.data.Chain
import cats.effect.{ConsoleOut, IO}
import cats.effect.concurrent.Ref
import cats.effect.test.TestConsole
import com.kubukoz.adventofcode2018.day4data.Event
import org.scalatest.{Matchers, WordSpec}

class Day4Tests extends WordSpec with Matchers {
  "mostFrequentMinute" should {
    "work in case one minute is more frequent than others" in {
      Day4.mostFrequentMinute(
        List(
          LocalTime.of(10, 11),
          LocalTime.of(10, 10),
          LocalTime.of(11, 11),
          LocalTime.of(12, 11),
          LocalTime.of(10, 10)
        )
      ) shouldBe Some((11, 3))
    }

    "work in case there are only three minutes" in {
      Day4.mostFrequentMinute(
        List(LocalTime.of(11, 11), LocalTime.of(12, 10), LocalTime.of(10, 10))
      ) shouldBe Some((10, 2))
    }
  }

  "part1, part2" should {
    "work in the example" in {
      val raw = """[1518-11-01 00:00] Guard #10 begins shift
                     |[1518-11-01 00:05] falls asleep
                     |[1518-11-01 00:25] wakes up
                     |[1518-11-01 00:30] falls asleep
                     |[1518-11-01 00:55] wakes up
                     |[1518-11-01 23:58] Guard #99 begins shift
                     |[1518-11-02 00:40] falls asleep
                     |[1518-11-02 00:50] wakes up
                     |[1518-11-03 00:05] Guard #10 begins shift
                     |[1518-11-03 00:24] falls asleep
                     |[1518-11-03 00:29] wakes up
                     |[1518-11-04 00:02] Guard #99 begins shift
                     |[1518-11-04 00:36] falls asleep
                     |[1518-11-04 00:46] wakes up
                     |[1518-11-05 00:03] Guard #99 begins shift
                     |[1518-11-05 00:45] falls asleep
                     |[1518-11-05 00:55] wakes up""".stripMargin

      val test = for {
        events <- fs2.Stream
          .emits(raw.split("\n"))
          .through(atto.fs2.Pipes.parseN[IO, Event](Day4.parser))
          .compile
          .toList

        result <- Day4.part1[IO](events)
        result2 <- Day4.part2[IO](events)
      } yield {
        result shouldBe Some(240)
        result2 shouldBe Some(4455)
      }

      test.unsafeRunSync()
    }
  }
}
