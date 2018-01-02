package com.kubukoz.adventofcode2017

import com.softwaremill.quicklens._
import fastparse.all._

import scala.annotation.tailrec

object Day23 {

  case class State(values: Map[Char, Long], mulsInvoked: Long) {
    def show(): Unit = {
      println(values.toList.sortBy(_._1))
    }

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

  case class Set(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ => value.valueIn(s))
    }
  }
  case class Sub(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ - value.valueIn(s))
    }
  }
  case class Mul(reg: Char, value: Pointer) extends Instruction {
    override val modifyState: State => State = { s =>
      s.update(reg, _ * value.valueIn(s)).modify(_.mulsInvoked).using(_ + 1)
    }
  }
  case class Jnz(value1: Pointer, value2: Pointer) extends Instruction {
    override val modifyState: State => State = identity
  }

  private val parse: String => Instruction = {
      val char: Parser[Char] = P(" ".rep ~ CharIn('a' to 'h').! ~ " ".rep).map(_.head)
      val number: Parser[Constant] = P(
        " ".rep ~ ("-".? ~ CharIn('0' to '9').rep(1)).! ~ " ".rep
      ).map(_.toLong).map(Constant)

      val pointer: Parser[Pointer] = P(
        char.map(Register) | number
      )

      val setPat = P("set" ~ char ~ pointer).map(Set.tupled)
      val addPat = P("sub" ~ char ~ pointer).map(Sub.tupled)
      val mulPat = P("mul" ~ char ~ pointer).map(Mul.tupled)
      val jnzPat = P("jnz" ~ pointer ~ pointer).map(Jnz.tupled)

      setPat | addPat | mulPat | jnzPat
  }.parse(_).get.value

  def solve(init: State, instructions: List[Instruction]): State = {
    @tailrec
    def go(state: State, index: Long): State = {
      instructions.drop(index.toInt).headOption match {
        case None =>
          state
        case Some(Jnz(reg, ptr)) if reg.valueIn(state) != 0 =>
          go(state, index + ptr.valueIn(state))
        case Some(h) =>
          go(h.modifyState(state), index + 1)
      }
    }

    go(init, 0)
  }


  def solve2(b: Long, c: Long): Int = {
    (b to c by 17).count { b =>
      (2L until b).exists { d =>
        (2.0d until b.toDouble by 1.0).contains(b / d.toDouble)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val fileInput = fileLines("/day23.txt")

    val parsed = fileInput.map(parse)

    val init1 = State(('a' to 'h').map(_ -> 0L).toMap, 0)

    println(solve(init1, parsed).mulsInvoked)
    println(solve2(108400L, 125400L))
  }
}
