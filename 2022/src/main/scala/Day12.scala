import scala.collection.mutable

object Day12 extends DailyChallenge[Int, Int] {
  final val Steps = List((1, 0), (-1, 0), (0, 1), (0, -1))

  def bfs(start: (Int, Int), connected: ((Int, Int), (Int, Int)) => Boolean): Map[(Int, Int), Int] = {
    val dists = mutable.Map.empty[(Int, Int), Int]
    val q = mutable.Queue.empty[(Int, Int)]

    dists(start) = 0
    q.enqueue(start)

    while (q.nonEmpty) {
      val (i, j) = q.dequeue()

      Steps.foreach({ case (di, dj) =>
        val ni = i + di
        val nj = j + dj
        if (!dists.contains((ni, nj)) && connected((i, j), (ni, nj))) {
          dists((ni, nj)) = dists((i, j)) + 1
          q.enqueue((ni, nj))
        }
      })
    }

    dists.toMap
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.trim).toVector

    val height = grid.length
    val width = grid(0).length

    def cellHeight(i: Int, j: Int): Char =
      if (grid(i)(j) == 'S') 'a'
      else if (grid(i)(j) == 'E') 'z'
      else grid(i)(j)

    def inGrid(i: Int, j: Int): Boolean =
      i >= 0 && i < height && j >= 0 && j < width

    val start =
      (0 until height).flatMap(i => (0 until width).map(j => (i, j))).find({ case (i, j) => grid(i)(j) == 'S' }).get
    val finish =
      (0 until height).flatMap(i => (0 until width).map(j => (i, j))).find({ case (i, j) => grid(i)(j) == 'E' }).get

    val distsFromStart =
      bfs(start, { case ((fi, fj), (ti, tj)) => inGrid(ti, tj) && cellHeight(ti, tj) - cellHeight(fi, fj) <= 1 })

    val distsFromFinish =
      bfs(finish, { case ((fi, fj), (ti, tj)) => inGrid(ti, tj) && cellHeight(fi, fj) - cellHeight(ti, tj) <= 1 })

    val part1 = distsFromStart(finish)
    val part2 = distsFromFinish.filter({ case ((i, j), _) => cellHeight(i, j) == 'a' }).values.min

    (part1, part2)
  }
}
