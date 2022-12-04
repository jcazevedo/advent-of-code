object Day04 extends DailyChallenge[Int, Int] {
  case class Interval(from: Int, to: Int) {
    def fullyContains(other: Interval): Boolean = other.from >= from && other.to <= to
    def overlaps(other: Interval): Boolean = if (from <= other.from) other.from <= to else from <= other.to
  }

  def run(input: String): (Int, Int) = {
    val intervalPairs = input
      .split("\n")
      .map(line => {
        val Array(i1, i2) = line.split(",")
        val Array(f1, t1) = i1.split("-")
        val Array(f2, t2) = i2.split("-")
        (Interval(f1.toInt, t1.toInt), Interval(f2.toInt, t2.toInt))
      })
      .toList

    val part1 = intervalPairs.count({ case (i1, i2) => i1.fullyContains(i2) || i2.fullyContains(i1) })
    val part2 = intervalPairs.count({ case (i1, i2) => i1.overlaps(i2) })

    (part1, part2)
  }
}
