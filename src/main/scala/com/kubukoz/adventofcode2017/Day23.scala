package com.kubukoz.adventofcode2017

import java.nio.file.{Files, Paths, StandardOpenOption}

import com.softwaremill.quicklens._

import scala.annotation.tailrec
import fastparse.all._

import scala.io.Source

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

//  Source.fromFile("./instructions.txt").getLines().mkString("\n")
  var i = 0
  def write(ins: Instruction, index: Long) = {
    import collection.JavaConverters._
    Files.write(Paths.get("./instructions.txt"), List(s"$i $index").asJava, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    i+=1
  }

  def solve(init: State, instructions: List[Instruction]): State = {
    @tailrec
    def go(state: State, index: Long): State = {
      val nextInstruction = instructions.drop(index.toInt).headOption

      nextInstruction.foreach(write(_, index))

      nextInstruction match {
        case None =>
          state
//        case Some(h@Jnz(Register('g'), Constant(-8))) =>
//          go(state.modify(_.values).using(_ ++ List('f' -> 0L, 'g' -> 0L, 'e' -> state.values('b'))), index + 1)

        case Some(h@Jnz(reg, ptr)) if reg.valueIn(state) != 0 =>
          go(state, index + ptr.valueIn(state))

        case Some(h) =>
          go(h.modifyState(state), index + 1)
      }
    }

    go(init, 0)
  }
  //(501, 1001)

  def main(args: Array[String]): Unit = {
    val fileInput = fileLines("/day23.txt")

    val parsed = fileInput.map(parse)

    val init1 = State(('a' to 'h').map(_ -> 0L).toMap, 0)
    val init2 = init1.modify(_.values).using(_ + ('a' -> 1L))

    parsed foreach println
//    println(solve(init1, parsed).mulsInvoked)
    println(solve(init2, parsed).values('h'))
  }
}
