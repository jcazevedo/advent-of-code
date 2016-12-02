package net.jcazevedo.adventofcode

class Day21 extends DailyChallenge[Int, Int] {
  case class Item(cost: Int, damage: Int, armor: Int)
  case class Player(hitPoints: Int, damage: Int, armor: Int) {
    def apply(items: Seq[Item]): Player = {
      val nextDamage = items.map(_.damage).sum
      val nextArmor = items.map(_.armor).sum
      this.copy(damage = this.damage + nextDamage, armor = this.armor + nextArmor)
    }
  }

  def winningPlayer(player1: Player, player2: Player): Player = {
    val players = Array(player1, player2)
    val hitPoints = Array(player1.hitPoints, player2.hitPoints)
    var current = 0

    while (hitPoints.forall(_ > 0)) {
      val attacker = current
      val defender = current ^ 1
      val damage = math.max(players(attacker).damage - players(defender).armor, 1)
      hitPoints(defender) -= damage

      current = current ^ 1
    }

    if (hitPoints(0) <= 0)
      players(1)
    else
      players(0)
  }

  def chooseWeapon(items: Seq[Item]): Seq[Seq[Item]] =
    items.combinations(1).toSeq

  def chooseArmor(items: Seq[Item]): Seq[Seq[Item]] =
    (items.combinations(0).toSeq ++ items.combinations(1).toSeq)

  def chooseRings(items: Seq[Item]): Seq[Seq[Item]] =
    (items.combinations(0).toSeq ++ items.combinations(1).toSeq ++ items.combinations(2).toSeq)

  def run(filename: String): (Int, Int) = {
    val stats = io.Source.fromFile(filename).getLines.toList.map { line =>
      val Array(name, value) = line.split(": ")
      name -> value.toInt
    }.toMap

    val boss = Player(
      stats.getOrElse("Hit Points", 0),
      stats.getOrElse("Damage", 0),
      stats.getOrElse("Armor", 0))

    val weapons = Seq(
      Item(8, 4, 0),
      Item(10, 5, 0),
      Item(25, 6, 0),
      Item(40, 7, 0),
      Item(74, 8, 0))

    val armor = Seq(
      Item(13, 0, 1),
      Item(31, 0, 2),
      Item(53, 0, 3),
      Item(75, 0, 4),
      Item(102, 0, 5))

    val rings = Seq(
      Item(25, 1, 0),
      Item(50, 2, 0),
      Item(100, 3, 0),
      Item(20, 0, 1),
      Item(40, 0, 2),
      Item(80, 0, 3))

    val items = for {
      weapons <- chooseWeapon(weapons)
      armors <- chooseArmor(armor)
      rings <- chooseRings(rings)
    } yield weapons ++ armors ++ rings

    val minCost = items.filter { li =>
      val p = Player(100, 0, 0).apply(li)
      winningPlayer(p, boss) == p
    }.map { li => li.map(_.cost).sum }.min

    val maxCost = items.filter { li =>
      val p = Player(100, 0, 0).apply(li)
      winningPlayer(p, boss) == boss
    }.map { li => li.map(_.cost).sum }.max

    (minCost, maxCost)
  }
}
