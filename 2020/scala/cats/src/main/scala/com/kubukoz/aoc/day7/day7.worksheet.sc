import cats.data.NonEmptyList
import com.kubukoz.aoc._
import cats.implicits._

case class Tree[A](value: A, children: List[Tree[A]]) {

  def pathToMatch(matcher: Tree[A] => Boolean): List[NonEmptyList[Tree[A]]] =
    if (matcher(this)) List(NonEmptyList.one(this))
    else
      children match {
        case Nil          => Nil
        case moreChildren =>
          moreChildren
            .flatMap(_.pathToMatch(matcher))
            .map(this :: _)
      }

  def find(pred: A => Boolean): Option[Tree[A]] =
    if (pred(value)) this.some
    else children.view.flatMap(_.find(pred)).headOption

  def fold[B](f: (A, List[B]) => B): B =
    f(value, children.map(_.fold(f)))

}

case class ChildReference(capacity: Int, kind: String)

def stripS(bags: String) = bags match {
  case s"$kind bags" => kind
  case s"$kind bag"  => kind
}

def parseValue(value: String): List[ChildReference] = {
  val pat = """(\d+) (.+)""".r

  value match {
    case "no other bags" => Nil
    case str             =>
      str
        .split(",")
        .map(_.trim)
        .map { case pat(n, bags) =>
          ChildReference(n.toInt, stripS(bags))
        }
        .toList
  }
}

val input: Map[String, List[ChildReference]] = {
  val pat = """(.+) contain (.+)\.""".r

  Util
    .readFileUnsafe(
      "./files/day7.txt"
    )
    .map { case pat(parent, children) =>
      stripS(parent) -> parseValue(children)
    }
    .toMap
}

val trees: List[Tree[ChildReference]] = {
  //all input keys that don't appear in values
  val topLevel = input.keySet -- input.values.flatten.map(_.kind)

  def buildNode(parent: ChildReference): Tree[ChildReference] = {
    val directChildren = input.getOrElse(parent.kind, Nil)

    Tree(parent, directChildren.map(buildNode))
  }

  topLevel.toList.map(ChildReference(0, _)).map(buildNode)
}

val part1 = {
  val possiblePathsToMe = trees.flatMap(_.pathToMatch(_.value.kind == "shiny gold"))

  possiblePathsToMe
    .flatMap(_.init)
    .map(_.value.kind)
    .distinct
    .size
}

val part2 = {
  val me = trees.flatMap(_.find(_.kind === "shiny gold")).head

  me.children.foldMap {
    _.fold[Int] { (child, itsChildren) =>
      child.capacity * (itsChildren.sum + 1)
    }
  }
}
