package adventofcode

class Day05 extends DailyChallenge[Int, Int] {
  def seatId(str: String): Int =
    str.foldLeft(0) { case (curr, ch) => curr * 2 + (if (ch == 'B' || ch == 'R') 1 else 0) }

  def run(input: String): (Int, Int) = {
    val seatIds = input.split("\n").map(seatId).toSet
    (seatIds.max, (seatIds.min to seatIds.max).find(!seatIds(_)).get)
  }
}
