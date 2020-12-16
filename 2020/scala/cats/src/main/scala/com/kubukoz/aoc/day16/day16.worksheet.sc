import cats.data.State
import com.kubukoz.aoc.Util
import cats.implicits._
case class Field(label: String, values: Set[Int])

val Array(fieldsString, myTicketString, nearbyTicketsString) = Util
  .readFileUnsafe("./files/day16.txt")
  .mkString("\n")
  .split("your ticket:|nearby tickets:")
  .map(_.trim)

val fields = {
  val range = """(\d+)\-(\d+)"""
  val pattern = s"""(.+): $range or $range""".r

  fieldsString.split("\n").map { case pattern(label, from1, to1, from2, to2) =>
    Field(label, ((from1.toInt to to1.toInt) ++ (from2.toInt to to2.toInt)).toSet)
  }
}

val myTicket = myTicketString.split(",").map(_.toInt).toVector

val nearbyTickets =
  nearbyTicketsString
    .split("\n")
    .map(_.split(",").map(_.toInt))
    .map(_.toList)
    .toList

def errors(ticket: List[Int]) = ticket
  .view
  .filter { field =>
    !fields.exists(_.values(field))
  }

//part 1 result
val errorRate = nearbyTickets.foldMap(errors(_).sum)

val validTickets = nearbyTickets.filter(errors(_).isEmpty).map(_.toVector)

//for every field, we have the list of potential indices...
val candidates = fields.map { field =>
  field -> fields.indices.filter { index =>
    (myTicket :: validTickets).forall(ticket => field.values(ticket(index)))
  }
}

//names of rules and the indices
val actualRules: Map[String, Int] =
  candidates
    .sortBy(_._2.size)
    .toList
    .map(_.leftMap(_.label))
    .nested
    .traverse { potentialValues =>
      State { (seen: Set[Int]) =>
        val notSeen = potentialValues.toSet -- seen
        require(notSeen.size == 1) //thank heck! that was one way to make that assumption

        val next = notSeen.head
        (seen + next, next)
      }
    }
    .runA(Set.empty)
    .value
    .value
    .toMap

//this is the actual part 2 result lol
actualRules
  .collect { case (k, v) if k.startsWith("departure") => v }
  .map(myTicket(_).toLong)
  .product
