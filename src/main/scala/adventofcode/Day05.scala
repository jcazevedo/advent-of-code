package adventofcode

class Day05 extends DailyChallenge[Int, Int] {
  def seat(str: String): (Int, Int) = {
    val row = str.take(7).foldLeft(0) {
      case (curr, 'F') => curr * 2 + 0
      case (curr, 'B') => curr * 2 + 1
    }
    val col = str.drop(7).foldLeft(0) {
      case (curr, 'L') => curr * 2 + 0
      case (curr, 'R') => curr * 2 + 1
    }
    (row, col)
  }

  def seatId(str: String): Int = {
    val (row, col) = seat(str)
    row * 8 + col
  }

  def run(input: String): (Int, Int) = {
    val seats = input.split("\n").toList
    val seatIds = seats.map(seatId).toSet
    (seatIds.max, (seatIds.min to seatIds.max).find(!seatIds(_)).get)
  }
}
