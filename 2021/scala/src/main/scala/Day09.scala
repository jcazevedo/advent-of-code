import scala.collection.mutable

object Day09 extends DailyChallenge[Int, Int] {
  def lowPoints(grid: Vector[String]): List[(Int, Int)] =
    (for {
      r <- (0 until grid.length)
      c <- (0 until grid(r).length)
      if r == 0 || grid(r - 1)(c) > grid(r)(c)
      if r + 1 == grid.length || grid(r + 1)(c) > grid(r)(c)
      if c == 0 || grid(r)(c - 1) > grid(r)(c)
      if c + 1 == grid(r).length || grid(r)(c + 1) > grid(r)(c)
    } yield (r, c)).toList

  def riskLevel(grid: Vector[String], point: (Int, Int)): Int =
    (grid(point._1)(point._2) - '0') + 1

  def basinSize(grid: Vector[String], point: (Int, Int)): Int = {
    val visited = mutable.Set.empty[(Int, Int)]

    def aux(curr: (Int, Int)): Int =
      if (visited.contains(curr) || grid(curr._1)(curr._2) == '9') 0
      else {
        var ans = 1
        visited.add(curr)
        val r = curr._1
        val c = curr._2

        if (r > 0 && grid(r - 1)(c) > grid(r)(c))
          ans += aux((r - 1, c))

        if (r + 1 < grid.length && grid(r + 1)(c) > grid(r)(c))
          ans += aux((r + 1, c))

        if (c > 0 && grid(r)(c - 1) > grid(r)(c))
          ans += aux((r, c - 1))

        if (c + 1 < grid(r).length && grid(r)(c + 1) > grid(r)(c))
          ans += aux((r, c + 1))

        ans
      }

    aux(point)
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").toVector

    val part1 = lowPoints(grid).map(riskLevel(grid, _)).sum
    val part2 = lowPoints(grid).map(basinSize(grid, _)).sorted.takeRight(3).reduce(_ * _)

    (part1, part2)
  }
}
