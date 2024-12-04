object Day04 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val grid = input.split("\n")

    val Rows = grid.length
    val Cols = grid(0).length
    val Coords = for {
      i <- 0 until Rows
      j <- 0 until Cols
    } yield (i, j)
    val Diffs = for {
      di <- -1 to 1
      dj <- -1 to 1
      if di != 0 || dj != 0
    } yield (di, dj)

    def good(ij: (Int, Int), d: (Int, Int), word: String): Boolean =
      word.isEmpty || (ij._1 >= 0 && ij._1 < Rows && ij._2 >= 0 && ij._2 < Cols &&
        grid(ij._1)(ij._2) == word.head &&
        good((ij._1 + d._1, ij._2 + d._2), d, word.tail))

    val part1 = Coords
      .map(ij => Diffs.count(d => good(ij, d, "XMAS")))
      .sum

    val part2 = {
      val DiagDiffs = Diffs.filter((di, dj) => math.abs(di) + math.abs(dj) == 2)
      Coords
        .count((i, j) =>
          DiagDiffs.exists(d1 =>
            DiagDiffs.exists(d2 =>
              d1 != d2 &&
                good((i + d1._1, j + d1._2), (-d1._1, -d1._2), "MAS") &&
                good((i + d2._1, j + d2._2), (-d2._1, -d2._2), "MAS")
            )
          )
        )
    }

    (part1, part2)
  }
}
