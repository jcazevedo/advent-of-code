package adventofcode

class Day09 extends DailyChallenge[Long, Long] {
  def good(preamble: Vector[Long], num: Long): Boolean = (for {
    v1 <- preamble
    v2 <- preamble
    if v2 != v1
  } yield v1 + v2).contains(num)

  def part1(numbers: Vector[Long]): Long =
    numbers.sliding(26).collectFirst { case preamble :+ num if !good(preamble, num) => num }.get

  def part2(numbers: Vector[Long], target: Long) = {
    val totSum = numbers
      .foldLeft(Vector(0L)) { case (before :+ last, num) =>
        before :+ last :+ (last + num)
      }
      .zipWithIndex
    (for {
      (sumR, r) <- totSum
      (sumL, l) <- totSum
      if l + 1 < r && (sumR - sumL) == target
      slice = numbers.slice(l + 1, r + 1)
    } yield slice.min + slice.max).head
  }

  def run(input: String): (Long, Long) = {
    val numbers = input.split("\n").map(_.toLong).toVector
    val ans1 = part1(numbers)
    (ans1, part2(numbers, ans1))
  }
}
