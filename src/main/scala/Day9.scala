

object Day9 {
  def main(args: Array[String]) {
    val input =
      """AlphaCentauri to Snowdin = 66
        |AlphaCentauri to Tambi = 28
        |AlphaCentauri to Faerun = 60
        |AlphaCentauri to Norrath = 34
        |AlphaCentauri to Straylight = 34
        |AlphaCentauri to Tristram = 3
        |AlphaCentauri to Arbre = 108
        |Snowdin to Tambi = 22
        |Snowdin to Faerun = 12
        |Snowdin to Norrath = 91
        |Snowdin to Straylight = 121
        |Snowdin to Tristram = 111
        |Snowdin to Arbre = 71
        |Tambi to Faerun = 39
        |Tambi to Norrath = 113
        |Tambi to Straylight = 130
        |Tambi to Tristram = 35
        |Tambi to Arbre = 40
        |Faerun to Norrath = 63
        |Faerun to Straylight = 21
        |Faerun to Tristram = 57
        |Faerun to Arbre = 83
        |Norrath to Straylight = 9
        |Norrath to Tristram = 50
        |Norrath to Arbre = 60
        |Straylight to Tristram = 27
        |Straylight to Arbre = 81
        |Tristram to Arbre = 90""".stripMargin.split("\n")

    val pattern = "(.+) to (.+) = (.+)".r

    val passages = input.map {
      case pattern(from, to, dist) => Route(from, to, dist.toInt)
    }.toList

    def passageLength(from: String, to: String) = {
      val s = passages.collect {
        case Route(f, t, length) if f == from && t == to => length
        case Route(t, f, length) if f == from && t == to => length
      }
      if (s.isEmpty) println(s"NO KURWA. $from to $to")
      s.head
    }
    val cities = passages.flatMap(s => List(s.from, s.to)).distinct

    val allRoutes: List[List[String]] = cities.permutations.toList

    val lengths: List[Int] = for {
      route <- allRoutes
    } yield (for {
      cityIndex <- route.indices
      city = route(cityIndex)
    } yield
      if (cityIndex == 0) 0
      else passageLength(route(cityIndex - 1), city)).sum


    println(s"min: ${lengths.min}, max: ${lengths.max}")
  }
}

case class Route(from: String, to: String, dist: Int)