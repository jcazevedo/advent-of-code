object Day04 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val grid = input.split("\n")

    val Rows = grid.length
    val Cols = grid(0).length
    val Coords = for {
      i <- 0 until Rows
      j <- 0 until Cols
    } yield (i, j)

    def good(ij: (Int, Int), d: (Int, Int), word: String): Boolean =
      if (word.isEmpty) true
      else if (ij._1 < 0 || ij._1 >= Rows || ij._2 < 0 || ij._2 >= Cols) false
      else if (grid(ij._1)(ij._2) != word.head) false
      else good((ij._1 + d._1, ij._2 + d._2), d, word.tail)

    val part1 = Coords
      .flatMap((i, j) =>
        for {
          di <- -1 to 1
          dj <- -1 to 1
          if di != 0 || dj != 0
          if good((i, j), (di, dj), "XMAS")
        } yield (di, dj)
      )
      .length

    val part2 = Coords
      .filter((i, j) =>
        (for {
          di1 <- -1 to 1
          dj1 <- -1 to 1
          if math.abs(di1) + math.abs(dj1) == 2
          if good((i + di1, j + dj1), (-di1, -dj1), "MAS")
          di2 <- -1 to 1
          dj2 <- -1 to 1
          if math.abs(di2) + math.abs(dj2) == 2
          if (di1, dj1) != (di2, dj2)
          if good((i + di2, j + dj2), (-di2, -dj2), "MAS")
        } yield ((di1, dj1), (di2, dj2))).nonEmpty
      )
      .length

    (part1, part2)
  }
}
