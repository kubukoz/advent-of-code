import com.kubukoz.aoc.Util
import cats.data.NonEmptyList
import cats.Monad
import cats.parse.Parser1
import cats.Defer
import cats.parse.Parser
import cats.parse.{Parser => P}
import com.kubukoz.aoc.Util
import cats.implicits._

sealed trait Expr extends Product with Serializable {

  def foldLeft[A](add: (A, A) => A, multiply: (A, A) => A, constant: Int => A): A = {
    def rec(e: Expr) = e.foldLeft(add, multiply, constant)

    this match {
      case Expr.Add(a, b)       => add(rec(a), rec(b))
      case Expr.Multiply(a, b)  => multiply(rec(a), rec(b))
      case Expr.Constant(value) => constant(value)
    }
  }

  def evaluated: Long = foldLeft[Long](_ + _, _ * _, _.toLong)

  def stringify: String = foldLeft[String]((a, b) => s"($a + $b)", (a, b) => s"($a * $b)", _.toString)
}

object Expr {
  case class Add(left: Expr, right: Expr) extends Expr
  case class Multiply(left: Expr, right: Expr) extends Expr
  case class Constant(value: Int) extends Expr
}

sealed trait Token extends Product with Serializable

object Token {
  case object LeftP extends Token
  case object RightP extends Token
  case class Number(value: Int) extends Token
  case object Plus extends Token
  case object Times extends Token
}

def findEnd(tokens: List[Token], level: Int): Int = tokens.head match {
  case Token.RightP => if (level == 1) 0 else 1 + findEnd(tokens.tail, level - 1)
  case Token.LeftP  => 1 + findEnd(tokens.tail, level + 1)
  case _            => 1 + findEnd(tokens.tail, level)
}

def decode(tokens: List[Token], history: List[(Expr, ((Expr, Expr) => Expr))]): Expr = {
  def decodeOperatorSequence(previous: Expr, rest: List[Token]): Expr = rest.headOption match {
    case None =>
      val allOperators = history.map(_._2)
      val allOperands = history.map(_._1) :+ previous

      //adding the current operand to the end and skipping the head
      val shiftedHistory = allOperands.tail.zip(allOperators)

      shiftedHistory.foldLeft(allOperands.head) { case (l, (r, op)) =>
        op(l, r)
      }

    case Some(next) =>
      next match {
        case Token.Plus  =>
          decode(rest.tail, history :+ (previous -> Expr.Add))
        case Token.Times =>
          decode(rest.tail, history :+ (previous -> Expr.Multiply))
        case _           => throw new Exception("Invalid decoding state")
      }
  }

  tokens.head match {
    case Token.LeftP =>
      val endOfParenIndex = findEnd(tokens, 0)
      val node = decode(tokens.slice(1, endOfParenIndex), Nil) //fresh history because of a new paren
      val rest = tokens.drop(endOfParenIndex + 1)

      decodeOperatorSequence(node, rest)

    case Token.Number(n) =>
      val node = Expr.Constant(n)

      decodeOperatorSequence(node, tokens.tail)
    case _               => throw new Exception("Invalid decoding state")
  }
}

def parser: Parser[Expr] = {
  import cats.parse._

  val tokens: Parser1[List[Token]] = P
    .oneOf1(
      List(
        P.char('(').as(Token.LeftP),
        P.char(')').as(Token.RightP),
        Numbers.digits1.map(_.toInt).map(Token.Number),
        P.char('+').as(Token.Plus),
        P.char('*').as(Token.Times)
      )
    )
    .map(_.some)
    .orElse1(P.char(' ').as(None))
    .rep1
    .map(_.toList.flatten)

  tokens.map(decode(_, Nil))
}

Util.readFileUnsafe("./files/day18.txt").map(parser.parseAll(_).getOrElse(???)).foldMap(_.evaluated)

// parser.parseAll("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").getOrElse(???).evaluated
