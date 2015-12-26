import scala.annotation.tailrec

object Day21 {
  def main(args: Array[String]) {
    val shop =
      """Weapons:    Cost  Damage  Armor
        |Dagger        8     4       0
        |Shortsword   10     5       0
        |Warhammer    25     6       0
        |Longsword    40     7       0
        |Greataxe     74     8       0
        |
        |Armor:      Cost  Damage  Armor
        |Leather      13     0       1
        |Chainmail    31     0       2
        |Splintmail   53     0       3
        |Bandedmail   75     0       4
        |Platemail   102     0       5
        |
        |Rings:      Cost  Damage  Armor
        |Damage +1    25     1       0
        |Damage +2    50     2       0
        |Damage +3   100     3       0
        |Defense +1   20     0       1
        |Defense +2   40     0       2
        |Defense +3   80     0       3""".stripMargin.replaceAll( """ \+([0-9])""", "_+$1").split("\n")


    val weapons = shop.slice(1, 6).map(Item.from)
    val armor = shop.slice(8, 13).map(Item.from)
    val rings = shop.slice(15, 21).map(Item.from)

    val allCombs = weapons.combinations(1).flatMap {
      wcomb => (armor.combinations(0) ++ armor.combinations(1)).flatMap {
        arcomb => (rings.combinations(0) ++ rings.combinations(1) ++ rings.combinations(2)).map(ringz => wcomb.toList ::: arcomb.toList ::: ringz.toList)
      }
    }.map(Stock).toList

    val input = Player(104, 8, 1)

    //part 1
    println(allCombs.sortBy(_.totalPrice).find(_.player.canDefeat(input)).map(_.totalPrice))
    //part 2
    println(allCombs.sortBy(-_.totalPrice).find(!_.player.canDefeat(input)).map(_.totalPrice))
  }
}

case class Player(hp: Int, dam: Int, armor: Int){
  def attack(another: Player) = another.copy(hp = another.hp - Math.max(1, dam - another.armor))
  val dead = hp <= 0
  @tailrec
  final def canDefeat(enemy: Player): Boolean = {
    if (dead) false
    else{
      val enemyAttacked = attack(enemy)
      if(enemyAttacked.dead) true
      else{
        enemyAttacked.attack(this).canDefeat(enemyAttacked)
      }
    }
  }
}


case class Stock(items: List[Item]){
  def damage = items.map(_.damage).sum
  def armor = items.map(_.armor).sum
  val player = Player(100, damage, armor)

  def totalPrice = items.map(_.cost).sum
}

case class Item(name: String, cost: Int, damage: Int, armor: Int)

object Item {
  def from(s: String) = s.split(" ").filterNot(_ == "") match {
    case Array(name, cost, damage, armor) => Item(name, cost.toInt, damage.toInt, armor.toInt)
  }
}
