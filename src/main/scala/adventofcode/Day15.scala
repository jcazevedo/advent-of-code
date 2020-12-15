package adventofcode

import scala.annotation.tailrec

class Day15 extends DailyChallenge[Int, Int] {
  @tailrec
  private def go(
      target: Int,
      numbers: Vector[Int],
      n: Int = 1,
      lastSpoken: Int = -1,
      cache: Map[Int, Int] = Map.empty
  ): Int = {
    val toSpeak =
      if (n <= numbers.length) numbers(n - 1)
      else cache.get(lastSpoken).fold(0)(x => n - x - 1)

    if (n == target) toSpeak else go(target, numbers, n + 1, toSpeak, cache.updated(lastSpoken, n - 1))
  }

  def part1(numbers: Vector[Int]): Int = go(2020, numbers)

  def part2(numbers: Vector[Int]): Int = go(30000000, numbers)

  def run(input: String): (Int, Int) = {
    val numbers = input.split(",").map(_.trim.toInt).toVector
    (part1(numbers), part2(numbers))
  }
}
