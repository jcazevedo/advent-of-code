package adventofcode

class Day03 extends DailyChallenge[Long, Long] {
  val slopes = List(
    (1, 1),
    (1, 3),
    (1, 5),
    (1, 7),
    (2, 1)
  )

  def part1(grid: Vector[String], di: Int, dj: Int): Long = {
    def aux(i: Int, j: Int): Long = {
      val ni = i + di
      if (ni >= grid.length)
        0L
      else {
        val nj = (j + dj) % grid(i).length
        (if (grid(ni)(nj) == '#') 1L else 0L) + aux(ni, nj)
      }
    }
    aux(0, 0)
  }

  def part2(grid: Vector[String]): Long = {
    slopes.map { case (di, dj) =>
      part1(grid, di, dj)
    }.product
  }

  def run(input: String): (Long, Long) = {
    val grid = input.split("\n").toVector
    (part1(grid, 1, 3), part2(grid))
  }
}
