package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2017.Day9._
import org.scalatest.{FlatSpec, Matchers}

class Day9Tests extends FlatSpec with Matchers {

  "parsing garbage" should "work" in {
    parse("<>") shouldBe Garbage("")
    parse("<lt7ali7li7>") shouldBe Garbage("lt7ali7li7")
    parse("<<<<>") shouldBe Garbage("<<<")
    parse("<{!>}>") shouldBe Garbage("{}")
    parse("<!!>") shouldBe Garbage("")
    parse("<!!!>>") shouldBe Garbage("")
    parse("""<{o"i!a,<{i<a>""") shouldBe Garbage("""{o"i,<{i<a""")
  }

  "parsing groups" should "work" in {
    parse("{}") shouldBe Group(Nil)
    parse("{{{}}}") shouldBe Group(Group(Group(Nil) :: Nil) :: Nil)
    parse("{{},{}}") shouldBe Group(Group(Nil) :: Group(Nil) :: Nil)
    parse("{{{},{},{{}}}}") shouldBe Group(Group(Group(Nil) :: Group(Nil) :: Group(Group(Nil) :: Nil) :: Nil) :: Nil)
    parse("{<{},{},{{}}>}") shouldBe Group(Garbage("{},{},{{}}") :: Nil)
    parse("{<a>,<a>,<a>,<a>}") shouldBe Group(List(Garbage("a"), Garbage("a"), Garbage("a"), Garbage("a")))
    parse("{{<a>},{<a>},{<a>},{<a>}}") shouldBe Group(List(
      Group(Garbage("a") :: Nil),
      Group(Garbage("a") :: Nil),
      Group(Garbage("a") :: Nil),
      Group(Garbage("a") :: Nil)
    ))
    parse("{{<!>},{<!>},{<!>},{<a>}}") shouldBe Group(Group(Garbage("},{<},{<},{<a") :: Nil) :: Nil)
  }

  "scoring" should "work" in {
    score("{}") shouldBe 1
    score("{{{}}}") shouldBe 6
    score("{{},{}}") shouldBe 5
    score("{{{},{},{{}}}}") shouldBe 16
    score("{<a>,<a>,<a>,<a>}") shouldBe 1
    score("{{<ab>},{<ab>},{<ab>},{<ab>}}") shouldBe 9
    score("{{<!!>},{<!!>},{<!!>},{<!!>}}") shouldBe 9
    score("{{<a!>},{<a!>},{<a!>},{<ab>}}") shouldBe 3
  }
}
