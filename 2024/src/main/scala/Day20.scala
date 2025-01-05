import scala.collection.mutable

object Day20 extends DailyChallenge[Int, Int] {
  val Diffs = List((1, 0), (0, 1), (-1, 0), (0, -1))

  def getDists(grid: Vector[String]): Map[(Int, Int), Int] = {
    val H = grid.length
    val W = grid(0).length

    var s = (-1, -1)
    (1 until H - 1).foreach(i => (1 until W - 1).foreach(j => if (grid(i)(j) == 'S') s = (i, j)))

    val q = mutable.Queue.empty[(Int, Int)]
    val dist = mutable.Map.empty[(Int, Int), Int]
    q.enqueue(s)
    dist(s) = 0

    while (q.nonEmpty) {
      val curr @ (i, j) = q.dequeue()

      Diffs.foreach((di, dj) => {
        val ni = i + di
        val nj = j + dj

        if (grid(ni)(nj) != '#') {
          val next = (ni, nj)
          if (!dist.contains(next)) {
            dist(next) = dist(curr) + 1
            q.enqueue(next)
          }
        }
      })
    }

    dist.toMap
  }

  def numCheats(grid: Vector[String], dists: Map[(Int, Int), Int], cheatTime: Int, diff: Int): Int = {
    val H = grid.size
    val W = grid(0).size

    var s = (-1, -1)
    var e = (-1, -1)
    (1 until H - 1).foreach(i =>
      (1 until W - 1).foreach(j =>
        if (grid(i)(j) == 'S') s = (i, j)
        else if (grid(i)(j) == 'E') e = (i, j)
      )
    )

    val noCheatDist = dists(e)

    val teleports = for {
      si <- (1 until H - 1)
      sj <- (1 until W - 1)
      if grid(si)(sj) != '#'
      ei <- (1 until H - 1)
      ej <- (1 until W - 1)
      if math.abs(si - ei) + math.abs(sj - ej) <= cheatTime
      if grid(ei)(ej) != '#'
    } yield ((si, sj), (ei, ej))

    teleports.count((from, to) => {
      val teleportLength = math.abs(from._1 - to._1) + math.abs(from._2 - to._2)
      noCheatDist - (dists(from) + (dists(e) - dists(to)) + teleportLength) >= diff
    })
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").toVector
    val dists = getDists(grid)
    val part1 = numCheats(grid, dists, cheatTime = 2, diff = 100)
    val part2 = numCheats(grid, dists, cheatTime = 20, diff = 100)
    (part1, part2)
  }
}
