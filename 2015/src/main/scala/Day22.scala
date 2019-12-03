object Day22 {
  class ModeSwitch(hard: Boolean)
  case object EasyMode extends ModeSwitch(true)
  case object HardMode extends ModeSwitch(true)
  def main(args: Array[String]) {
    val enemy = Enemy(hp = 71, damage = 10, poisonedTurns = 0)
    val player = Player(enemy, hp = 50, mana = 500, manaSpent = 0, shieldTurns = 0, rechargeTurns = 0)
    println(player.getWinningOptions.map(_.manaSpent).min)

    implicit val mode = HardMode
    val playerPart2 = Player(enemy, hp = 50, mana = 500, manaSpent = 0, shieldTurns = 0, rechargeTurns = 0)

    println(playerPart2.getWinningOptions.map(_.manaSpent).min)
  }

  case class Player(enemy: Enemy, hp: Int, mana: Int, manaSpent: Int, shieldTurns: Int, rechargeTurns: Int)(implicit val mode: ModeSwitch = EasyMode) {
    val spells = List(MagicMissile, Drain, Shield, Poison, Recharge)

    val armor = if (shieldTurns == 0) 0 else 7

    def dead = hp <= 0

    def takeAttack = if (!enemy.dead) {
      val damage = 1 max (enemy.damage - armor)
      copy(hp = hp - damage)
    } else this

    def copySpendingMana(i: Int): Player = copy(mana = mana - i, manaSpent = manaSpent + i)

    def turn(spell: Spell): Player = {
      val newMe = mode match{
        case EasyMode => this
        case HardMode =>
          val temp = copy(hp = hp - 1)
          if(temp.dead) return temp
          temp
      }
      val afterEffects = newMe.applyEffectsIfAny.copy(enemy = enemy.applyEffectsIfAny)
      val afterCast = spell match {
        case Shield => afterEffects.copySpendingMana(Shield.mana).copy(shieldTurns = 6)
        case Recharge => afterEffects.copySpendingMana(Recharge.mana).copy(rechargeTurns = 5)
        case Drain => afterEffects.copySpendingMana(Drain.mana).copy(hp = afterEffects.hp + 2)
        case _ => afterEffects.copySpendingMana(spell.mana)
      }
      val afterAttacking = afterCast.copy(enemy = spell match {
        case MagicMissile => afterCast.enemy.takeAttack(4)
        case Drain => afterCast.enemy.takeAttack(2)
        case Poison => afterCast.enemy.poison
        case _ => afterCast.enemy
      })
      val afterEnemyStarted = afterAttacking.applyEffectsIfAny.copy(enemy = afterAttacking.enemy.applyEffectsIfAny)
      afterEnemyStarted.takeAttack
    }

    def copyRecharged = {
      if (rechargeTurns > 0) copy(mana = mana + 101, rechargeTurns = rechargeTurns - 1) else this
    }

    def copyShielded = if (shieldTurns > 0) copy(shieldTurns = shieldTurns - 1) else this

    def applyEffectsIfAny: Player = copyRecharged.copyShielded

    def canAfford(s: Spell) = s.mana <= mana

    def canMove: PartialFunction[Spell, Boolean] = {
      case m@MagicMissile if canAfford(m) => true
      case d@Drain if canAfford(d) => true
      case s@Shield if canAfford(s) && shieldTurns == 0 => true
      case p@Poison if canAfford(p) && enemy.poisonedTurns == 0 => true
      case r@Recharge if canAfford(r) && rechargeTurns == 0 => true
    }

    def options: List[Player] =
      spells.filter(applyEffectsIfAny.copy(enemy = enemy.applyEffectsIfAny).canMove.isDefinedAt).map(turn)

    def getWinningOptions: List[Player] = dead match {
      case false if enemy.dead => this :: Nil
      case false => options.flatMap(_.getWinningOptions)
      case _ => Nil
    }
  }

  object Poison extends Spell(173)

  object MagicMissile extends Spell(53)

  object Drain extends Spell(73)

  object Shield extends Spell(113)

  object Recharge extends Spell(229)

  case class Spell(mana: Int)

  case class Enemy(hp: Int, damage: Int, poisonedTurns: Int) {
    def applyEffectsIfAny: Enemy = poisonedTurns match {
      case 0 => this
      case _ => copy(hp = hp - 3, poisonedTurns = poisonedTurns - 1)
    }

    val dead = hp <= 0

    def takeAttack(dmg: Int) = copy(hp = hp - dmg)

    def poison = copy(poisonedTurns = 6)
  }
}
