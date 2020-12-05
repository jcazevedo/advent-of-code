package adventofcode

class Day05 extends DailyChallenge[Int, Int] {
  def seatId(str: String): Int = {
    str.foldLeft(0) {
      case (curr, 'F' | 'L') => curr * 2 + 0
      case (curr, 'B' | 'R') => curr * 2 + 1
    }
  }

  def run(input: String): (Int, Int) = {
    val seats = input.split("\n").toList
    val seatIds = seats.map(seatId).toSet
    (seatIds.max, (seatIds.min to seatIds.max).find(!seatIds(_)).get)
  }
}
