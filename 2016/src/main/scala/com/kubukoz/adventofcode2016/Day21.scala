package com.kubukoz.adventofcode2016

object Day21 {
  sealed trait Direction {
    val reverse: Direction
  }

  case object Left extends Direction {
    override val reverse: Direction = Right
  }
  case object Right extends Direction {
    override val reverse: Direction = Left
  }

  sealed trait Instruction extends Product with Serializable {
    def transform(s: String): String
    def invert: Instruction
  }

  case class SwapPosition(x: Int, y: Int) extends Instruction {
    override def transform(s: String): String = {
      s
        .updated(x, s(y))
        .updated(y, s(x))
    }

    override def invert: Instruction = this
  }

  case class SwapLetters(x: Char, y: Char) extends Instruction {
    override def transform(s: String): String = {
      SwapPosition(s.indexOf(x), s.indexOf(y)).transform(s)
    }

    override def invert: Instruction = this
  }

  case class Rotate(direction: Direction, steps: Int) extends Instruction {
    override def transform(s: String): String = {
      val newSteps = steps % s.length
      val splitPoint = direction match {
        case Left => newSteps
        case Right => s.length - newSteps
      }

      val (left, right) = s.splitAt(splitPoint)
      right + left
    }

    override def invert: Instruction = Rotate(direction.reverse, steps)
  }

  case class Move(x: Int, y: Int) extends Instruction {
    override def transform(s: String): String = {
      val (beforeX, atX :: afterX) = s.toList.splitAt(x)

      val removedX = (beforeX ::: afterX).mkString

      val (beforeY, sinceY) = removedX.splitAt(y)

      beforeY + atX + sinceY
    }

    override def invert: Instruction = Move(y, x)
  }

  case class RotateOnPosition(x: Char) extends Instruction {
    private def additionalStep(index: Int): Int = if (index >= 4) 1 else 0

    override def transform(s: String): String = {
      val index = s.indexOf(x)

      Rotate(Right, 1 + index + additionalStep(index)).transform(s)
    }

    override def invert: Instruction = Reverse

    object Reverse extends RotateOnPosition(x) {
      override def transform(s: String): String = {
        //nice brute force here :D
        s.indices.map(Rotate(Left, _))
          .map(_.transform(s))
          .find(invert.transform(_) == s).get
      }

      override def invert: Instruction = RotateOnPosition.this

      override def toString: String = s"ReverseRotateOnPosition($x)"
    }
  }

  case class ReversePositions(x: Int, y: Int) extends Instruction {
    override def transform(s: String): String = {
      val (beforeX, sinceX) = s.splitAt(x)
      val len = (y + 1) - x

      val toReverse = sinceX.take(len)
      beforeX + toReverse.reverse + sinceX.drop(len)
    }

    override def invert: Instruction = this
  }

  def parse(input: List[String]): List[Instruction] = {
    val rotatePattern = """rotate (\w+) (\d+) steps?""".r
    val movePattern = """move position (\d+) to position (\d+)""".r
    val swapLettersPattern = """swap letter (\w) with letter (\w)""".r
    val swapPositionsPattern = """swap position (\d+) with position (\d+)""".r
    val rotateBasedPattern = """rotate based on position of letter (\w)""".r
    val reversePositionsPattern = """reverse positions (\d+) through (\d+)""".r

    input.map {
      case rotatePattern("right", steps) => Rotate(Right, steps.toInt)
      case rotatePattern("left", steps) => Rotate(Left, steps.toInt)
      case movePattern(x, y) => Move(x.toInt, y.toInt)
      case swapLettersPattern(x, y) => SwapLetters(x.head, y.head)
      case swapPositionsPattern(x, y) => SwapPosition(x.toInt, y.toInt)
      case rotateBasedPattern(x) => RotateOnPosition(x.head)
      case reversePositionsPattern(x, y) => ReversePositions(x.toInt, y.toInt)
    }
  }

  def scramble(password: String, instructions: List[Instruction]): List[String] = {
    instructions.scanLeft(password) { (str, instr) =>
      val transformed = instr.transform(str)
      println(s"$str -> $instr -> $transformed")
      transformed
    }
  }

  def unscramble(hash: String, instructions: List[Instruction]): List[String] = {
    scramble(hash, instructions.reverseMap(_.invert))
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day21.txt")
    val instructions: List[Instruction] = parse(input)

    val password = "abcdefgh"

    val scrambled = scramble(password, instructions)
    println()
    val unscrambled = unscramble("fbgdceah", instructions)
    println("Result: " + scrambled.last)
    println("Unscrambled: " + unscrambled.last)
  }
}
