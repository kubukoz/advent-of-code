package com.kubukoz.adventofcode2017

import com.softwaremill.quicklens._

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Day18 {
  case class State(values: Map[Char, Long], playedFrequencies: List[Long]) {
    def update(ch: Char, value: Long => Long): State = {
      copy(values = values.updated(ch, value(values(ch))))
    }
  }

  sealed trait Pointer extends Product with Serializable {
    def valueIn(state: State): Long = this match {
      case Constant(x)    => x
      case Register(name) => state.values(name)
    }
  }

  case class Constant(x: Long) extends Pointer
  case class Register(name: Char) extends Pointer

  sealed trait Instruction extends Product with Serializable {
    val modifyState: State => State
  }

  case class Snd(value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.modify(_.playedFrequencies).using(value.valueIn(s) :: _)
    }
  }
  case class Set(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ => value.valueIn(s))
    }
  }
  case class Add(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ + value.valueIn(s))
    }
  }
  case class Mul(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ * value.valueIn(s))
    }
  }
  case class Mod(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ % value.valueIn(s))
    }
  }
  case class Rcv(reg: Char) extends Instruction {
    override val modifyState: State => State = identity
  }
  case class Jgz(value1: Pointer, value2: Pointer) extends Instruction {
    override val modifyState: State => State = identity
  }

  private val anything = """(.+)"""
  private val char = """(\w)"""

  private val sndPat = s"snd $anything".r
  private val setPat = s"set $char $anything".r
  private val addPat = s"add $char $anything".r
  private val mulPat = s"mul $char $anything".r
  private val modPat = s"mod $char $anything".r
  private val rcvPat = s"rcv $char".r
  private val jgzPat = s"jgz $anything $anything".r

  private val parsePointer: String => Pointer = s =>
    Try(s.toInt) match {
      case Success(i) => Constant(i)
      case _          => Register(s.head)
  }

  private val parse: String => Instruction = {
    case sndPat(ptr)        => Snd(parsePointer(ptr))
    case setPat(reg, value) => Set(reg.head, parsePointer(value))
    case addPat(reg, value) => Add(reg.head, parsePointer(value))
    case mulPat(reg, value) => Mul(reg.head, parsePointer(value))
    case modPat(reg, value) => Mod(reg.head, parsePointer(value))
    case rcvPat(reg)        => Rcv(reg.head)
    case jgzPat(reg, value) => Jgz(parsePointer(reg), parsePointer(value))
  }

  def solve(instructions: List[Instruction]): Long = {
    @tailrec
    def go(state: State, index: Long): Long =
      instructions.drop(index.toInt) match {
        case Rcv(x) :: _ if state.values(x) != 0 =>
          state.playedFrequencies.head

        case Jgz(reg, ptr) :: _ =>
          val indexDelta = if (reg.valueIn(state) > 0) ptr.valueIn(state) else 1
          go(state, index + indexDelta)

        case h :: _ =>
          go(h.modifyState(state), index + 1)
      }

    go(State(Map.empty.withDefaultValue(0), Nil), 0)
  }

  type Inbox = Vector[Long]
  type Outbox = Option[Long]

  case class Info(state: State,
                  status: Option[Status],
                  inbox: Inbox,
                  outbox: Outbox,
                  index: Int,
                  name: String,
                  everSentValues: Long)

  sealed trait Status extends Product with Serializable
  case object Waiting extends Status
  case object Terminated extends Status

  def solve2(instructions: List[Instruction]): Long = {
    @tailrec
    def go(info: Info): Info = {
      instructions.drop(info.index) match {
        case _ if info.status.contains(Terminated) => info
        case Rcv(_) :: _ if info.inbox.isEmpty =>
          info.copy(status = Some(Waiting))

        case Rcv(x) :: _ =>
          go(
            info
              .modify(_.state).using(Set(x, Constant(info.inbox.head)).modifyState)
              .modify(_.status).setTo(None)
              .modify(_.inbox).using(_.tail)
              .modify(_.index).using(_ + 1)
          )

        case Snd(x) :: _ =>
          info
            .modify(_.outbox).setTo(Some(x.valueIn(info.state)))
            .modify(_.index).using(_ + 1)
            .modify(_.everSentValues).using(_ + 1)

        case Jgz(reg, ptr) :: _ =>
          val indexDelta =
            if (reg.valueIn(info.state) > 0) ptr.valueIn(info.state) else 1
          go(info.modify(_.index).using(x => (x + indexDelta).toInt))

        case h :: _ =>
          go(
            info
              .modify(_.state).using(h.modifyState)
              .modify(_.index).using(_ + 1)
          )
        case Nil => info.copy(status = Some(Terminated))
      }
    }

    @tailrec
    def go2(
        state1: Info,
        state2: Info,
        n: Int
    ): (Info, Info) = {
      val run1 = go(state1)
      val run2 = go(state2)

      val terminated1 = run1.status.contains(Terminated) || (
        run1.status.contains(Waiting) && run2.outbox.isEmpty
      )
      val terminated2 = run2.status.contains(Terminated) || (
        run2.status.contains(Waiting) && run1.outbox.isEmpty
      )

      val shouldStopNow = terminated1 && terminated2

      if (shouldStopNow) {
        (run1, run2)
      } else
        go2(
          run1
            .modify(_.inbox).using(_ ++ run2.outbox.toVector)
            .modify(_.outbox).setTo(None),
          run2
            .modify(_.inbox).using(_ ++ run1.outbox.toVector)
            .modify(_.outbox).setTo(None),
          n + 1
        )
    }

    locally {
      val init = State(Map.empty.withDefaultValue(0), Nil)
      val initInfo = Info(init, None, Vector.empty, None, 0, "state1", 0)

      val (_, state2) = go2(
        initInfo,
        initInfo.copy(name = "state2").modify(_.state.values).using(_.updated('p', 1)),
        1
      )
      state2.everSentValues
    }
  }

  def main(args: Array[String]): Unit = {
    val fileInput = fileLines("/day18.txt")

    val parsed = fileInput.map(parse)

    println(solve(parsed))
    println(solve2(parsed))
  }
}
