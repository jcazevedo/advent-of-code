object Day07 extends DailyChallenge[Int, Int] {
  def sumTo(n: Int): Int =
    (n * (n + 1)) / 2

  def run(input: String): (Int, Int) = {
    val positions = input.trim.split(",").map(_.toInt).toList

    val part1 = (positions.min to positions.max).map(target => positions.map(p => math.abs(target - p)).sum).min
    val part2 = (positions.min to positions.max).map(target => positions.map(p => sumTo(math.abs(target - p))).sum).min

    (part1, part2)
  }
}
