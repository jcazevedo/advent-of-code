object Day11 extends DailyChallenge[Long, Long] {
  case class Test(divisibleBy: Long, monkeyIfTrue: Int, monkeyIfFalse: Int) {
    def target(item: Item): Int = if (item.remBy(divisibleBy) == 0) monkeyIfTrue else monkeyIfFalse
  }

  sealed trait Item {
    def remBy(v: Long): Long
    def map(f: Long => Long): Item
  }

  case class WithRelief(item: Long) extends Item {
    def remBy(v: Long): Long = item % v
    def map(f: Long => Long): Item = WithRelief(f(item) / 3)
  }

  case class WithoutRelief(rems: Map[Long, Long]) extends Item {
    def remBy(v: Long): Long = rems(v)
    def map(f: Long => Long): Item = WithoutRelief(rems.map({ case (k, v) => k -> f(v) % k }))
  }

  case class Monkey(id: Int, items: Vector[Item], operation: Long => Long, test: Test, inspected: Int = 0)

  def monkeyBusiness(monkeys: Map[Int, Monkey], nRounds: Int): Long = {
    def runRound(m: Map[Int, Monkey]): Map[Int, Monkey] = {
      def aux(curr: Map[Int, Monkey], i: Int): Map[Int, Monkey] =
        curr.get(i) match {
          case None => curr
          case Some(monkey) =>
            val next = monkey.items
              .foldLeft(curr)((acc, wl) => {
                val nwl = wl.map(monkey.operation)
                val target = monkey.test.target(nwl)
                acc.updated(target, acc(target).copy(items = acc(target).items :+ nwl))
              })
              .updated(i, monkey.copy(items = Vector.empty, inspected = monkey.inspected + monkey.items.size))
            aux(next, i + 1)
        }

      aux(m, 0)
    }

    (0 until nRounds)
      .foldLeft(monkeys)((curr, _) => runRound(curr))
      .values
      .map(_.inspected.toLong)
      .toList
      .sorted
      .takeRight(2)
      .product
  }

  def run(input: String): (Long, Long) = {
    val monkeyR = "^Monkey (\\d+):$".r
    val startingItemsR = "^Starting items: (.+)$".r
    val oppMultR = "^Operation: new = old \\* (.+)$".r
    val oppAddR = "^Operation: new = old \\+ (.+)$".r
    val testR = "^Test: divisible by (.+)$".r
    val ifTrueR = "^If true: throw to monkey (.+)$".r
    val ifFalseR = "^If false: throw to monkey (.+)$".r

    val monkeysWithRelief = input
      .split("\n")
      .map(_.trim)
      .foldLeft(List.empty[Monkey])({ case (curr, line) =>
        line match {
          case monkeyR(id) => Monkey(id.toInt, Vector.empty, identity, Test(-1, -1, -1)) :: curr
          case startingItemsR(itemsS) =>
            curr.head.copy(items = itemsS.split(", ").map(_.toLong).toVector.map(WithRelief.apply)) :: curr.tail
          case oppMultR(y) =>
            (if (y == "old") curr.head.copy(operation = x => x * x)
             else curr.head.copy(operation = x => x * y.toInt)) :: curr.tail
          case oppAddR(y)  => curr.head.copy(operation = x => x + y.toInt) :: curr.tail
          case testR(x)    => curr.head.copy(test = curr.head.test.copy(divisibleBy = x.toInt)) :: curr.tail
          case ifTrueR(x)  => curr.head.copy(test = curr.head.test.copy(monkeyIfTrue = x.toInt)) :: curr.tail
          case ifFalseR(x) => curr.head.copy(test = curr.head.test.copy(monkeyIfFalse = x.toInt)) :: curr.tail
          case ""          => curr
        }
      })
      .map(monkey => monkey.id -> monkey)
      .toMap

    val monkeysWithoutRelief = {
      val rems = monkeysWithRelief.values.map(_.test.divisibleBy).toList
      monkeysWithRelief.map({ case (k, v) =>
        k -> v.copy(items = v.items.map({
          case WithRelief(item)    => WithoutRelief(rems.map(rem => rem -> item % rem).toMap)
          case WithoutRelief(rems) => WithoutRelief(rems)
        }))
      })
    }

    val part1 = monkeyBusiness(monkeysWithRelief, 20)
    val part2 = monkeyBusiness(monkeysWithoutRelief, 10000)

    (part1, part2)
  }
}
