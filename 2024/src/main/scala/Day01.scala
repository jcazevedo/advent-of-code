object Day01 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val linePattern = "^([0-9]+) *([0-9]+)$".r

    val (left, right) = input
      .split("\n")
      .toList
      .collect({ case linePattern(l, r) => (l.toInt, r.toInt) })
      .unzip

    val part1 = left.sorted.zip(right.sorted).map({ case (l, r) => math.abs(l - r) }).sum

    val part2 = {
      val rightMap = right.groupMapReduce(identity)(_ => 1)(_ + _)
      left.map(l => l * rightMap.getOrElse(l, 0)).sum
    }

    (part1, part2)
  }
}
