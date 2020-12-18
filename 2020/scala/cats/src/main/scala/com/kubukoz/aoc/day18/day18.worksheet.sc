import cats.parse.Numbers
import cats.kernel.Order
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

sealed trait Operator extends Product with Serializable {

  def express(lhs: Expr, rhs: Expr): Expr = this match {
    case Operator.Add      => Expr.Add(lhs, rhs)
    case Operator.Multiply => Expr.Multiply(lhs, rhs)
  }

}

object Operator {
  case object Add extends Operator
  case object Multiply extends Operator

  implicit val order: Order[Operator] = Order.by(List(Add, Multiply).indexOf)
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

trait FoldOperands {
  def perform(firstOperand: Expr, operators: List[(Expr, Operator)]): Expr
}

object FoldOperands {

  val equalPrecedence: FoldOperands = (head, operators) =>
    operators.foldLeft(head) { case (l, (r, op)) =>
      op.express(l, r)
    }

  val addFirst: FoldOperands = (head, operators) => {
    operators.headOption match {
      case None => head

      case Some(_) =>
        val (adds, rest) = operators.span(_._2 == Operator.Add)

        // these are all additions so we can treat them equally
        val addsReduced = equalPrecedence.perform(head, adds)

        // next operator (if any) has to be multiplication so we check what's after it
        rest match {
          case Nil                               => addsReduced
          case (operand, multiplier) :: restTail =>
            // basically (add prefix) * (add suffix)
            multiplier
              .express(
                addsReduced,
                addFirst.perform(
                  operand,
                  restTail
                )
              )
        }
    }
  }

}

def decode(
  tokens: List[Token],
  history: List[(Expr, Operator)]
)(
  implicit folder: FoldOperands
): Expr = {
  def decodeOperatorSequence(previous: Expr, rest: List[Token]): Expr = rest.headOption match {
    case None =>
      val allOperators = history.map(_._2)
      val allOperands = history.map(_._1) :+ previous

      //adding the current operand to the end and skipping the head
      val shiftedHistory = allOperands.tail.zip(allOperators)

      folder.perform(allOperands.head, shiftedHistory)

    case Some(next) =>
      next match {
        case Token.Plus  =>
          decode(rest.tail, history :+ (previous -> Operator.Add))
        case Token.Times =>
          decode(rest.tail, history :+ (previous -> Operator.Multiply))
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

def tokenize(input: String): List[Token] = P
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
  .parseAll(input)
  .leftMap(_ => ???)
  .merge

val input = Util.readFileUnsafe("./files/day18.txt")

val tokens = input.map(tokenize)

List(FoldOperands.equalPrecedence, FoldOperands.addFirst).map { implicit folder =>
  tokens.foldMap(decode(_, Nil).evaluated)
}
