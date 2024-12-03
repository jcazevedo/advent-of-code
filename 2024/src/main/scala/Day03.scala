object Day03 extends DailyChallenge[Long, Long] {
  def run(input: String): (Long, Long) = {
    val mulRegex = "mul\\(([0-9]+),([0-9]+)\\)".r
    val instrRegex = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)".r

    val part1 = mulRegex
      .findAllIn(input)
      .collect({ case mulRegex(lhs, rhs) => lhs.toLong * rhs.toLong })
      .sum
    val part2 = instrRegex
      .findAllIn(input)
      .foldLeft((true, 0L))({
        case ((active, sum), "do()")           => (true, sum)
        case ((active, sum), "don't()")        => (false, sum)
        case ((true, sum), mulRegex(lhs, rhs)) => (true, sum + lhs.toLong * rhs.toLong)
        case (acc, _)                          => acc
      })
      ._2

    (part1, part2)
  }
}
