import scala.collection.mutable

object Day14 extends DailyChallenge[Int, Long] {
  def step(polymer: String, rules: Map[String, String]): String = {
    val res = mutable.StringBuilder()

    polymer.indices.foreach { idx =>
      res += polymer(idx)
      if (idx + 1 < polymer.length)
        res.append(rules.getOrElse(s"${polymer(idx)}${polymer(idx + 1)}", ""))
    }

    res.toString
  }

  def elementCounts(polymer: String): Map[Char, Int] =
    polymer.groupBy(identity).view.mapValues(_.size).toMap

  def finalCountOf(ch: Char, polymer: String, after: Int, rules: Map[String, String]): Long = {
    val cache = mutable.Map.empty[(String, Int), Long]

    def aux(c: String, i: Int): Long = {
      if (!cache.contains((c, i))) {
        if (i == 0) cache((c, i)) = c.count(_ == ch)
        else {
          val pairs = c.sliding(2).toList
          val rec = pairs
            .map(pair =>
              if (rules.contains(pair)) aux(s"${pair(0)}${rules(pair)}${pair(1)}", i - 1)
              else pair.count(_ == ch).toLong
            )
            .sum
          cache((c, i)) = rec - c.substring(1, c.length - 1).count(_ == ch)
        }
      }
      cache((c, i))
    }

    aux(polymer, after)
  }

  def run(input: String): (Int, Long) = {
    val lines = input.split("\n").map(_.trim).toList

    val polymer = lines.head
    val rules = lines
      .drop(2)
      .map(line => {
        val Array(from, to) = line.split(" -> ")
        from -> to
      })
      .toMap

    val after10Steps = (1 to 10).foldLeft(polymer)((curr, _) => step(curr, rules))
    val cnts = elementCounts(after10Steps)
    val part1 = cnts.values.max - cnts.values.min

    val validElements = polymer.toSet ++ rules.keySet.flatMap(_.toSet) ++ rules.values.flatMap(_.toSet).toSet
    val part2Cnts = validElements.toList.map(element => element -> finalCountOf(element, polymer, 40, rules)).toMap
    val part2 = part2Cnts.values.max - part2Cnts.values.min

    (part1, part2)
  }
}
