object Day19 extends DailyChallenge[Int, Long] {
  def ways(design: String, towels: Vector[String]): Long = {
    val N = design.length
    val dp = Array.fill(N + 1)(0L)

    dp(0) = 1L
    (1 to N).foreach(i =>
      towels.foreach(towel =>
        if (i - towel.size >= 0 && design.substring(i - towel.size, i) == towel)
          dp(i) += dp(i - towel.size)
      )
    )

    dp(N)
  }

  def run(input: String): (Int, Long) = {
    val (towels, designs) = {
      val lines = input.split("\n")
      val towels = lines(0).split(", ").toVector
      val designs = lines.drop(2).toVector
      (towels, designs)
    }

    val part1 = designs.count(ways(_, towels) != 0L)
    val part2 = designs.map(ways(_, towels)).sum

    (part1, part2)
  }
}
