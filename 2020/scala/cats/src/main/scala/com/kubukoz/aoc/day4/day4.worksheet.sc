import com.kubukoz.aoc._
import cats.implicits._

def parsePassport(raw: String): Map[String, String] = raw
  .split("""(\s+)""")
  .map(_.split(":").toList match {
    case a :: b :: Nil => (a, b)
    case idontcare     => throw new Exception(idontcare.toString)
  })
  .toMap

val requiredFields = Map[String, String => Boolean](
  "byr" -> { _.toIntOption.exists((1920 to 2002).contains) },
  "iyr" -> { _.toIntOption.exists((2010 to 2020).contains) },
  "eyr" -> { _.toIntOption.exists((2020 to 2030).contains) },
  "hgt" -> {
    case s"${height}cm" => height.toIntOption.exists((150 to 193).contains)
    case s"${height}in" => height.toIntOption.exists((59 to 76).contains)
    case _              => false
  },
  "hcl" -> { s =>
    s.startsWith("#") && s.length == 7 && s.tail.forall((('a' to 'f') ++ ('0' to '9')).toSet)
  },
  "ecl" -> "amb blu brn gry grn hzl oth".split(" ").toSet,
  "pid" -> { s => s.length == 9 && s.forall(_.isDigit) }
)

def isValid1(passport: Map[String, String]) =
  requiredFields.keySet.forall(passport.contains)

def isValid2(passport: Map[String, String]) =
  requiredFields.forall { case (key, condition) => passport.get(key).exists(condition) }

val input = Util.readFileUnsafe("./files/day4.txt").mkString("\n").split("\n\n").map(parsePassport).count(isValid2)
