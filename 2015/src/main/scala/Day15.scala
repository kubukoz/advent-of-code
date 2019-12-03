object Day15 {
  val input =
    """Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
      |Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
      |Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
      |Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1""".stripMargin.split("\n")

  val numPat = "(-?[0-9]+)"
  val pattern = s"([A-z]+): capacity $numPat, durability $numPat, flavor $numPat, texture $numPat, calories $numPat".r

  def main(args: Array[String]) {
    val ingredients = input.map { case pattern(name, cap, dur, flav, text, cal) => Ingredient(name, cap.toInt, dur.toInt, flav.toInt, text.toInt, cal.toInt) }.toList
    val spoons = 100
    val combinations = (0 to 100).combinations(ingredients.size).filter(_.sum <= spoons).flatMap(_.permutations).map { seq =>
      Recipe(seq.indices.map { i =>
        ingredients(i) -> seq(i)
      }.toMap)
    }.toList
    println("Part 1: " + combinations.map(_.totalScore).max)
    println("Part 2: " + combinations.filter(_.calorieSum == 500).map(_.totalScore).max)
  }
}

case class Ingredient(name: String, capacity: Int, duration: Int, flavor: Int, texture: Int, calories: Int)

case class Recipe(ingredients: Map[Ingredient, Int]) {
  val capSum = ingredients.map { case (i, am) => i.capacity * am }.sum
  val durSum = ingredients.map { case (i, am) => i.duration * am }.sum
  val flavorSum = ingredients.map { case (i, am) => i.flavor * am }.sum
  val textureSum = ingredients.map { case (i, am) => i.texture * am }.sum
  val calorieSum = ingredients.map { case (i, am) => i.calories * am }.sum

  def totalScore = List(capSum, durSum, flavorSum, textureSum).map(_ max 0).product
}