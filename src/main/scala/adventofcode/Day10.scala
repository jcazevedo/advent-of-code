package adventofcode

import scala.collection.mutable

class Day10 extends DailyChallenge[Int, Long] {
  def part1(adapters: List[Int]): Int = {
    val diffs = (0 :: adapters.sorted)
      .sliding(2)
      .map { case List(lo, hi) =>
        hi - lo
      }
      .toList
    diffs.count(_ == 1) * (diffs.count(_ == 3) + 1)
  }

  def part2(adapters: List[Int]): Long = {
    val available = adapters.toSet + 0 + adapters.max
    val ways = mutable.Map.empty[Int, Long]
    ways(available.max) = 1L

    def go(power: Int): Long = {
      if (!ways.contains(power)) {
        if (!available(power))
          ways(power) = 0L
        else
          ways(power) = go(power + 1) + go(power + 2) + go(power + 3)
      }
      ways(power)
    }

    go(0)
  }

  def run(input: String): (Int, Long) = {
    val adapters = input.split("\n").map(_.toInt).toList
    (part1(adapters), part2(adapters))
  }
}
