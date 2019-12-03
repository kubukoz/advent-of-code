import scala.annotation.tailrec

object Day19 {
  val input =
    """Al => ThF
      |Al => ThRnFAr
      |B => BCa
      |B => TiB
      |B => TiRnFAr
      |Ca => CaCa
      |Ca => PB
      |Ca => PRnFAr
      |Ca => SiRnFYFAr
      |Ca => SiRnMgAr
      |Ca => SiTh
      |F => CaF
      |F => PMg
      |F => SiAl
      |H => CRnAlAr
      |H => CRnFYFYFAr
      |H => CRnFYMgAr
      |H => CRnMgYFAr
      |H => HCa
      |H => NRnFYFAr
      |H => NRnMgAr
      |H => NTh
      |H => OB
      |H => ORnFAr
      |Mg => BF
      |Mg => TiMg
      |N => CRnFAr
      |N => HSi
      |O => CRnFYFAr
      |O => CRnMgAr
      |O => HP
      |O => NRnFAr
      |O => OTi
      |P => CaP
      |P => PTi
      |P => SiRnFAr
      |Si => CaSi
      |Th => ThCa
      |Ti => BP
      |Ti => TiTi
      |e => HF
      |e => NAl
      |e => OMg
      |
      |ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF""".stripMargin.split("\n")

  val transPat = "(.+) => (.+)".r

  def getTransforms(transformList: Array[(String, String)]) = transformList.map(_._1).distinct.map(from => (from, transformList.collect { case (from2, to) if from2 == from => to }.toSet)).toMap

  val transformList: Array[(String, String)] = input.dropRight(2).map {
    case transPat(from, to) => (from, to)
  }
  val transforms: Map[String, Set[String]] = getTransforms(transformList)
  val transformsSwapped: Map[String, Set[String]] = getTransforms(transformList.map(_.swap))
  val medicine = input.last

  @tailrec
  def options(s: String, transforms: Map[String, Set[String]], currentIndex: Int = 0, state: List[String] = Nil): List[String] = {
    val rightString = s.substring(currentIndex)
    val figuresLeft = transforms.keys.map(key => (key, rightString.indexOf(key))).filter(_._2 >= 0)
    val nextFigure = if (figuresLeft.isEmpty) None else Some(figuresLeft.minBy(_._2))
    nextFigure match {
      case Some((figure, ind)) => options(s, transforms, currentIndex + figure.length, state ::: transforms(figure).map { op =>
        val newop = s.substring(0, currentIndex + ind) + op + rightString.substring(ind + figure.length)
        newop
      }.toList)
      case None => state.distinct
    }
  }

  def main(args: Array[String]): Unit = {
    //part1
    println("Part 1: " + options(medicine, transforms).size)
    //part2
    var i = 0

    var result: List[String] = medicine :: Nil
    while (!result.contains("e")) {
      i += 1
      result = result.flatMap(op => options(op, transformsSwapped).distinct).distinct.sortBy(_.length).take(9) //really works with >= 9, but i tried it with 100 first
    }
    println(s"Part 2: It took $i rounds!")
  }
}
