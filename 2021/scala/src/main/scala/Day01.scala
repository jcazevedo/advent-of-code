object Day01 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val measurements = input.split("\n").map(_.toInt).toList

    val part1 = measurements.sliding(2).count {
      case List(m1, m2) => m2 > m1
      case _            => false
    }
    val part2 = measurements.sliding(3).map(_.sum).toList.sliding(2).count {
      case List(m1, m2) => m2 > m1
      case _            => false
    }

    (part1, part2)
  }
}
