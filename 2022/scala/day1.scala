//> using scala "3.2.1"
import scala.compiletime.ops.int.{>, +, *}
import scala.compiletime.ops.string.{Length, Substring, CharAt}
import Tuple.Map
import scala.NonEmptyTuple

object StringOps {
  type IndexOfRec[Haystack <: String, Needle <: String, I <: Int] <: Int =
    ((I + Length[Needle]) > Length[Haystack]) match {
      case true => -1
      case false =>
        Substring[Haystack, I, I + Length[Needle]] match {
          case Needle => I
          case _      => IndexOfRec[Haystack, Needle, I + 1]
        }
    }

  type IndexOf[Haystack <: String, Needle <: String] =
    IndexOfRec[Haystack, Needle, 0]

  type SplitRec[S <: String, Delim <: String, Memory <: Tuple] <: Tuple =
    IndexOf[S, Delim] match {
      case -1 => TupleOps.Reverse[S *: Memory]
      case Int =>
        SplitRec[
          Substring[S, IndexOf[S, Delim] + Length[Delim], Length[S]],
          Delim,
          Substring[S, 0, IndexOf[S, Delim]] *: Memory
        ]
    }

  type Split[S <: String, Delim <: String] =
    SplitRec[S, Delim, EmptyTuple]

  type ParseDigit[Ch <: Char] <: Int = Ch match {
    case '0' => 0
    case '1' => 1
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
  }

  type ToTuple[S <: String] <: Tuple = S match {
    case "" => EmptyTuple
    case _  => (S CharAt 0) *: ToTuple[Substring[S, 1, Length[S]]]
  }

  type ParseInt[S <: String] =
    Tuple.Fold[
      TupleOps.Reverse[
        ToTuple[S] Map
          ([A] =>> A match {
            case Char => ParseDigit[A]
          })
      ],
      0,
      [A, B] =>> (A, B) match {
        case (Int, Int) => A + (10 * B)
      }
    ]
}

object TupleOps {
  import Tuple._

  type Reverse[T <: Tuple] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case h *: t     => Tuple.Concat[Reverse[t], h *: EmptyTuple]
  }

  type Reduce[T <: NonEmptyTuple, F[_, _]] =
    Tuple.Fold[Tuple.Tail[T], Tuple.Head[T], F]

  type NonEmptyMap[T <: NonEmptyTuple, F[_]] =
    F[Tuple.Head[T]] *: Tuple.Map[Tuple.Tail[T], F]

  type Sum[T <: Tuple] = Tuple.Fold[
    T,
    0,
    [A, B] =>> (A, B) match {
      case (Int, Int) => A + B
    }
  ]

  type Maximum[T <: NonEmptyTuple] = Reduce[
    T,
    [A, B] =>> (A, B) match {
      case (Int, Int) => A Max B
    }
  ]

  type IndexOfRec[T <: Tuple, Elem, I <: Int] <: Int =
    Tuple.Elem[T, I] match {
      case Elem => I
      case _    => IndexOfRec[T, Elem, I + 1]
    }

  type IndexOf[T <: Tuple, Elem] = IndexOfRec[T, Elem, 0]

  type DropLargest[T <: NonEmptyTuple] =
    (
      (T Take (T IndexOf Maximum[T])) Concat
        (T Drop ((T IndexOf Maximum[T]) + 1))
    )

  type BubbleSort[T <: Tuple] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case NonEmptyTuple =>
      BubbleSort[DropLargest[T]] Concat (Maximum[T] *: EmptyTuple)
  }

  // don't judge, my input is like 3 elements long. I don't care that it's O(n^2)
  type Sort[T <: Tuple] = BubbleSort[T]

  type TopN[T <: Tuple, N <: Int] =
    Tuple.Fold[
      T,
      EmptyTuple,
      [Item, Acc] =>> (Item, Acc) match {
        case (Int, Tuple) =>
          (Size[Acc] < N) match {
            case true => Item *: Acc
            case false =>
              (Item > Maximum[Acc]) match {
                case true  => Reverse[Sort[(Item *: Acc)]] Take N
                case false => Acc
              }
          }
      }
    ]

}

object AdventOps {

  import Tuple._
  import TupleOps._

  type Groups[S <: String] = StringOps.Split[S, "\n\n"]

  type Elves[Groups <: Tuple] =
    Groups Map
      ([Group] =>> Group match {
        case String =>
          StringOps.Split[Group, "\n"] Map StringOps.ParseInt
      })

  type TopElf[Elves <: NonEmptyTuple] =
    TupleOps.Maximum[
      Elves NonEmptyMap
        ([Elf] =>> Elf match {
          case Tuple => TupleOps.Sum[Elf]
        }),
    ]

  type TopElvesPartial[Elves <: NonEmptyTuple] =
    TupleOps.TopN[
      Elves NonEmptyMap
        ([Elf] =>> Elf match {
          case Tuple => TupleOps.Sum[Elf]
        }),
      3
    ]

  type TopElves[Elves <: NonEmptyTuple] =
    TopElvesPartial[Elves] match {
      case Tuple =>
        Sum[TopElvesPartial[Elves]]
    }
}

// final val text = readFileNow(
//   "/Users/kubukoz/projects/advent-of-code/2022/input/day1.txt"
// )
final val text = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""
type S = text.type

import AdventOps._

type Parsed = Elves[Groups[S]]

object tests {
  // input size
  showType[Tuple.Size[Parsed]]

  def part1 =
    summon[TopElf[Parsed] =:= 24000]

  def part2 =
    summon[TopElves[Parsed] =:= 45000]
}
