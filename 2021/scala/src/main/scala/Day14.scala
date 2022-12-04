import scala.collection.mutable

object Day14 extends DailyChallenge[Long, Long] {
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

  def run(input: String): (Long, Long) = {
    val lines = input.split("\n").map(_.trim).toList

    val polymer = lines.head
    val rules = lines
      .drop(2)
      .map(line => {
        val Array(from, to) = line.split(" -> ")
        from -> to
      })
      .toMap

    val validElements = polymer.toSet ++ rules.keySet.flatMap(_.toSet) ++ rules.values.flatMap(_.toSet).toSet

    val cnts1 = validElements.toList.map(element => element -> finalCountOf(element, polymer, 10, rules)).toMap
    val part1 = cnts1.values.max - cnts1.values.min

    val cnts2 = validElements.toList.map(element => element -> finalCountOf(element, polymer, 40, rules)).toMap
    val part2 = cnts2.values.max - cnts2.values.min

    (part1, part2)
  }
}
