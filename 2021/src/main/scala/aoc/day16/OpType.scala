package aoc.day16

sealed trait OpType extends Product with Serializable {
  import OpType._

  private def cond(f: (Long, Long) => Boolean): (Long, Long) => Long =
    (a, b) =>
      if (f(a, b))
        1L
      else
        0L

  def eval: (Long, Long) => Long =
    this match {
      case Sum         => _ + _
      case Product     => _ * _
      case Minimum     => _ min _
      case Maximum     => _ max _
      case GreaterThan => cond(_ > _)
      case LessThan    => cond(_ < _)
      case EqualTo     => cond(_ == _)
    }

}

object OpType {
  case object Sum extends OpType
  case object Product extends OpType
  case object Minimum extends OpType
  case object Maximum extends OpType
  case object GreaterThan extends OpType
  case object LessThan extends OpType
  case object EqualTo extends OpType

  val values = Map(
    0 -> Sum,
    1 -> Product,
    2 -> Minimum,
    3 -> Maximum,
    //    4 -> Sike
    5 -> GreaterThan,
    6 -> LessThan,
    7 -> EqualTo,
  )

}
