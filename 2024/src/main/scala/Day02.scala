object Day02 extends DailyChallenge[Int, Int] {
  case class Report(levels: Vector[Int])

  def isSafe(report: Report): Boolean = {
    val pairs = report.levels.zip(report.levels.tail)
    val signum = math.signum(pairs.head._1 - pairs.head._2)
    pairs.forall({ case (n1, n2) =>
      math.signum(n1 - n2) == signum && {
        val diff = math.abs(n1 - n2)
        diff >= 1 && diff <= 3
      }
    })
  }

  def isSafeTolerant(report: Report): Boolean =
    isSafe(report) || report.levels.zipWithIndex.exists((_, i) =>
      isSafe(report.copy(levels = report.levels.take(i) ++ report.levels.drop(i + 1)))
    )

  def run(input: String): (Int, Int) = {
    val reports = input.split("\n").map(line => Report(line.split("\\s+").map(_.toInt).toVector)).toList

    val part1 = reports.count(isSafe)
    val part2 = reports.count(isSafeTolerant)

    (part1, part2)
  }
}
