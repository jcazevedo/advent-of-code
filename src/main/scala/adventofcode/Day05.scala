package adventofcode

class Day05 extends DailyChallenge[Int, Int] {
  def seatId(str: String): Int =
    str.foldLeft(0) { case (curr, ch) =>
      (ch: @unchecked) match {
        case 'F' | 'L' => curr * 2 + 0
        case 'B' | 'R' => curr * 2 + 1
      }
    }

  def run(input: String): (Int, Int) = {
    val seatIds = input.split("\n").map(seatId).toSet
    (seatIds.max, (seatIds.min to seatIds.max).find(!seatIds(_)).get)
  }
}
