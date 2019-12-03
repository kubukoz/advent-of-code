package com.kubukoz.adventofcode2017

import com.softwaremill.quicklens._

import scala.annotation.tailrec
import fastparse.all._

object Day18 {

  case class State(values: Map[Char, Long], playedFrequencies: List[Long]) {
    def update(ch: Char, value: Long => Long): State =
      copy(values = values.updated(ch, value(values(ch))))
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

  private val parse: String => Instruction = {
    val char: Parser[Char] = P(" ".rep ~ CharIn('a' to 'z').! ~ " ".rep).map(_.head)
    val number: Parser[Constant] = P(
      " ".rep ~ ("-".? ~ CharIn('0' to '9').rep(1)).! ~ " ".rep
    ).map(_.toLong).map(Constant)

    val pointer: Parser[Pointer] = P(
      char.map(Register) | number
    )

    val sndPat = P("snd" ~ pointer).map(Snd)
    val setPat = P("set" ~ char ~ pointer).map(Set.tupled)
    val addPat = P("add" ~ char ~ pointer).map(Add.tupled)
    val mulPat = P("mul" ~ char ~ pointer).map(Mul.tupled)
    val modPat = P("mod" ~ char ~ pointer).map(Mod.tupled)
    val rcvPat = P("rcv" ~ char).map(Rcv)
    val jgzPat = P("jgz " ~ pointer ~ pointer).map(Jgz.tupled)

    sndPat | setPat | addPat | mulPat | modPat | rcvPat | jgzPat
  }.parse(_).get.value

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

  def solve2(instructions: List[Instruction]): Long = {
    case class StateInfo(state: State,
                         status: Option[Status],
                         inbox: Vector[Long],
                         outbox: Option[Long],
                         index: Int,
                         name: String,
                         everSentValues: Long)

    sealed trait Status extends Product with Serializable
    case object Waiting extends Status
    case object Terminated extends Status

    @tailrec
    def go(info: StateInfo): StateInfo = {
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
        state1: StateInfo,
        state2: StateInfo,
        n: Int
    ): (StateInfo, StateInfo) = {
      val run1 = go(state1)
      val run2 = go(state2)

      val shouldStopNow = List(run1, run2).permutations.forall { perm =>
        perm.head.status.exists {
          case Terminated => true
          case Waiting if perm.last.outbox.isEmpty => true
          case _ => false
        }
      }

      def clear(info: StateInfo, appendOutbox: Option[Long]): StateInfo = {
        info
          .modify(_.inbox).using(_ ++ appendOutbox.toVector)
          .modify(_.outbox).setTo(None)
      }

      if (shouldStopNow) {
        (run1, run2)
      } else go2(clear(run1, run2.outbox), clear(run2, run1.outbox), n + 1)
    }

    locally {
      val init = State(Map.empty.withDefaultValue(0), Nil)
      val initInfo = StateInfo(init, None, Vector.empty, None, 0, "state1", 0)

      val (_, state2) = go2(
        initInfo,
        initInfo
          .modify(_.name).setTo("state2")
          .modify(_.state.values).using(_.updated('p', 1)),
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
