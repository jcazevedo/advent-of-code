object Day13 extends DailyChallenge[Long, Long] {
  case class Point(x: Long, y: Long)
  case class Prize(buttonA: Point, buttonB: Point, prize: Point)

  def minTokensToWin(prize: Prize): Long = {
    val det = prize.buttonA.x * prize.buttonB.y - prize.buttonB.x * prize.buttonA.y
    val a = (prize.buttonB.y * prize.prize.x - prize.buttonB.x * prize.prize.y) / det
    val b = (prize.buttonA.x * prize.prize.y - prize.buttonA.y * prize.prize.x) / det

    if (a < 0 || b < 0) 0
    else if (a * prize.buttonA.x + b * prize.buttonB.x != prize.prize.x) 0
    else if (a * prize.buttonA.y + b * prize.buttonB.y != prize.prize.y) 0
    else 3 * a + b
  }

  def run(input: String): (Long, Long) = {
    val prizeRegex =
      """Button A: X([+-][0-9]+), Y([+-][0-9]+)\nButton B: X([+-][0-9]+), Y([+-][0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)""".r

    val prizes = prizeRegex
      .findAllIn(input)
      .collect({ case prizeRegex(xa, ya, xb, yb, px, py) =>
        Prize(Point(xa.toLong, ya.toLong), Point(xb.toLong, yb.toLong), Point(px.toLong, py.toLong))
      })
      .toList

    val part1 = prizes
      .map(minTokensToWin)
      .sum
    val part2 = prizes
      .map(p => p.copy(prize = Point(10000000000000L + p.prize.x, 10000000000000L + p.prize.y)))
      .map(minTokensToWin)
      .sum

    (part1, part2)
  }
}
