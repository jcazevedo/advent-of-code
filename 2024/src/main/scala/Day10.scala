import scala.collection.mutable

object Day10 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val grid = input.split("\n")
    val Rows = grid.length
    val Cols = grid(0).length
    val Diffs = for {
      di <- -1 to 1
      dj <- -1 to 1
      if math.abs(di) + math.abs(dj) == 1
    } yield (di, dj)

    def inside(ij: (Int, Int)): Boolean =
      ij._1 >= 0 && ij._1 < Rows && ij._2 >= 0 && ij._2 < Cols

    def trailheadScore(start: (Int, Int), checkVisited: Boolean): Int = {
      val visited = mutable.Set.empty[(Int, Int)]
      val q = mutable.Queue.empty[(Int, Int)]
      visited.addOne(start)
      q.enqueue(start)

      var cnt: Int = 0
      while (!q.isEmpty) {
        val curr = q.dequeue()
        Diffs.foreach((di, dj) => {
          val ni = curr._1 + di
          val nj = curr._2 + dj
          if (
            inside((ni, nj)) &&
            grid(ni)(nj) == grid(curr._1)(curr._2) + 1 &&
            (!checkVisited || !visited.contains((ni, nj)))
          ) {
            visited.addOne((ni, nj))
            if (grid(ni)(nj) == '9') {
              cnt += 1
            } else {
              q.enqueue((ni, nj))
            }
          }
        })
      }
      cnt
    }

    val part1 =
      (0 until Rows)
        .map(i =>
          (0 until Cols).map(j => if (grid(i)(j) == '0') trailheadScore((i, j), checkVisited = true) else 0).sum
        )
        .sum
    val part2 =
      (0 until Rows)
        .map(i =>
          (0 until Cols).map(j => if (grid(i)(j) == '0') trailheadScore((i, j), checkVisited = false) else 0).sum
        )
        .sum

    (part1, part2)
  }
}
