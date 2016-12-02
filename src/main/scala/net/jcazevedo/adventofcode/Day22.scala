package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day22 extends DailyChallenge[Int, Int] {
  case class GameState(
      mana: Int,
      totalManaSpent: Int,
      playerHitPoints: Int,
      bossHitPoints: Int,
      bossDamage: Int,
      shieldEffect: Int,
      poisonEffect: Int,
      rechargeEffect: Int) {
    def doBossDamage: GameState = {
      val armor = if (shieldEffect > 0) 7 else 0
      val damage = math.max(bossDamage - armor, 1)
      this.copy(
        mana = this.mana + (if (rechargeEffect > 0) 101 else 0),
        playerHitPoints = this.playerHitPoints - damage,
        bossHitPoints = this.bossHitPoints - (if (poisonEffect > 0) 3 else 0),
        shieldEffect = math.max(this.shieldEffect - 1, 0),
        poisonEffect = math.max(this.poisonEffect - 1, 0),
        rechargeEffect = math.max(this.rechargeEffect - 1, 0))
    }

    def castMagicMissile: Option[GameState] = {
      if (mana >= 53) {
        Some(this.copy(
          mana = this.mana - 53 + (if (rechargeEffect > 0) 101 else 0),
          totalManaSpent = this.totalManaSpent + 53,
          bossHitPoints = this.bossHitPoints - 4 - (if (poisonEffect > 0) 3 else 0),
          shieldEffect = math.max(this.shieldEffect - 1, 0),
          poisonEffect = math.max(this.poisonEffect - 1, 0),
          rechargeEffect = math.max(this.rechargeEffect - 1, 0)))
      } else {
        None
      }
    }

    def castDrain: Option[GameState] = {
      if (mana >= 73) {
        Some(this.copy(
          mana = this.mana - 73 + (if (rechargeEffect > 0) 101 else 0),
          totalManaSpent = this.totalManaSpent + 73,
          playerHitPoints = this.playerHitPoints + 2,
          bossHitPoints = this.bossHitPoints - 2 - (if (poisonEffect > 0) 3 else 0),
          shieldEffect = math.max(this.shieldEffect - 1, 0),
          poisonEffect = math.max(this.poisonEffect - 1, 0),
          rechargeEffect = math.max(this.rechargeEffect - 1, 0)))
      } else {
        None
      }
    }

    def castShield: Option[GameState] = {
      if (mana >= 113 && shieldEffect <= 1) {
        Some(this.copy(
          mana = this.mana - 113 + (if (rechargeEffect > 0) 101 else 0),
          totalManaSpent = this.totalManaSpent + 113,
          bossHitPoints = this.bossHitPoints - (if (poisonEffect > 0) 3 else 0),
          shieldEffect = 6,
          poisonEffect = math.max(this.poisonEffect - 1, 0),
          rechargeEffect = math.max(this.rechargeEffect - 1, 0)))
      } else {
        None
      }
    }

    def castPoison: Option[GameState] = {
      if (mana >= 173 && poisonEffect <= 1) {
        Some(this.copy(
          mana = this.mana - 173 + (if (rechargeEffect > 0) 101 else 0),
          totalManaSpent = this.totalManaSpent + 173,
          bossHitPoints = this.bossHitPoints - (if (poisonEffect > 0) 3 else 0),
          shieldEffect = math.max(this.shieldEffect - 1, 0),
          poisonEffect = 6,
          rechargeEffect = math.max(this.rechargeEffect - 1, 0)))
      } else {
        None
      }
    }

    def castRecharge: Option[GameState] = {
      if (mana >= 229 && rechargeEffect <= 1) {
        Some(this.copy(
          mana = this.mana - 229 + (if (rechargeEffect > 0) 101 else 0),
          totalManaSpent = this.totalManaSpent + 229,
          bossHitPoints = this.bossHitPoints - (if (poisonEffect > 0) 3 else 0),
          shieldEffect = math.max(this.shieldEffect - 1, 0),
          poisonEffect = math.max(this.poisonEffect - 1, 0),
          rechargeEffect = 5))
      } else {
        None
      }
    }
  }

  def getLeastAmountOfMana(start: GameState): Int = {
    implicit val ordering: Ordering[GameState] = Ordering.by({ g: GameState => g.totalManaSpent }).reverse

    val pq = mutable.PriorityQueue[GameState]()
    pq += start

    while (!pq.isEmpty) {
      val current = pq.dequeue()
      if (current.bossHitPoints <= 0)
        return current.totalManaSpent

      if (current.playerHitPoints > 0) {
        val next = Seq(current.castMagicMissile, current.castDrain, current.castShield, current.castPoison, current.castRecharge).flatten
        next.map(_.doBossDamage).foreach { pq += _ }
      }
    }

    -1
  }

  def getLeastAmountOfManaHard(start: GameState): Int = {
    implicit val ordering: Ordering[GameState] = Ordering.by({ g: GameState => g.totalManaSpent }).reverse

    val pq = mutable.PriorityQueue[GameState]()
    pq += start

    while (!pq.isEmpty) {
      val t = pq.dequeue()
      val current = t.copy(playerHitPoints = t.playerHitPoints - 1)
      if (current.bossHitPoints <= 0)
        return current.totalManaSpent

      if (current.playerHitPoints > 0) {
        val next = Seq(current.castMagicMissile, current.castDrain, current.castShield, current.castPoison, current.castRecharge).flatten
        next.map(_.doBossDamage).foreach { pq += _ }
      }
    }

    -1
  }

  def run(filename: String): (Int, Int) = {
    val stats = io.Source.fromFile(filename).getLines.toList.map { line =>
      val Array(name, value) = line.split(": ")
      name -> value.toInt
    }.toMap

    val start =
      GameState(500, 0, 50, stats.getOrElse("Hit Points", 0), stats.getOrElse("Damage", 0), 0, 0, 0)

    (getLeastAmountOfMana(start), getLeastAmountOfManaHard(start))
  }
}
