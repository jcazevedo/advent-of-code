import scala.collection.mutable

object Day15 extends DailyChallenge[Long, Long] {
  val directions = Map('^' -> (-1, 0), '>' -> (0, 1), 'v' -> (1, 0), '<' -> (0, -1))

  def move(grid: Vector[String], ops: String): Vector[String] = {
    val g = grid.map(_.toArray).toArray
    val H = g.length
    val W = g(0).length

    var ri = -1
    var rj = -1
    var op = 0

    (0 until H).foreach(i =>
      (0 until W).foreach(j =>
        if (g(i)(j) == '@') {
          ri = i
          rj = j
        }
      )
    )

    while (op < ops.length) {
      var ni = ri + directions(ops(op))._1
      var nj = rj + directions(ops(op))._2

      if (g(ni)(nj) == 'O') {
        val pi = ni
        val pj = nj
        while (g(ni)(nj) == 'O') {
          ni = ni + directions(ops(op))._1
          nj = nj + directions(ops(op))._2
        }
        if (g(ni)(nj) == '.') {
          g(ni)(nj) = 'O'
          g(ri)(rj) = '.'
          g(pi)(pj) = '@'
          ri = pi
          rj = pj
        }
      } else if (g(ni)(nj) == '.') {
        g(ri)(rj) = '.'
        g(ni)(nj) = '@'
        ri = ni
        rj = nj
      }

      op = op + 1
    }

    g.map(_.mkString).toVector
  }

  def wideMove(grid: Vector[String], ops: String): Vector[String] = {
    val g = grid.map(_.toArray).toArray
    val H = g.length
    val W = g(0).length

    var ri = -1
    var rj = -1
    var op = 0

    (0 until H).foreach(i =>
      (0 until W).foreach(j =>
        if (g(i)(j) == '@') {
          ri = i
          rj = j
        }
      )
    )

    while (op < ops.length) {
      val di = directions(ops(op))._1
      val dj = directions(ops(op))._2

      var ni = ri + di
      var nj = rj + dj

      if (g(ni)(nj) == '[' || g(ni)(nj) == ']') {
        val pi = ni
        val pj = nj
        if (ops(op) == '<' || ops(op) == '>') {
          while (g(ni)(nj) == '[' || g(ni)(nj) == ']') {
            ni = ni + di
            nj = nj + dj
          }
          if (g(ni)(nj) == '.') {
            while (ni != pi || nj != pj) {
              g(ni)(nj) = g(ni - di)(nj - dj)
              ni = ni - di
              nj = nj - dj
            }
            g(ri)(rj) = '.'
            g(pi)(pj) = '@'
            ri = pi
            rj = pj
          }
        } else {
          val q = mutable.Queue.empty[(Int, Int)]
          val boxes = mutable.ListBuffer.empty[(Int, Int)]

          if (g(ni)(nj) == '[') q.enqueue((ni, nj))
          else q.enqueue((ni, nj - 1))

          var move = true
          while (q.nonEmpty && move) {
            val (i, j) = q.dequeue()
            boxes.addOne((i, j))
            if (g(i + di)(j) == '#' || g(i + di)(j + 1) == '#')
              move = false
            else if (g(i + di)(j) == '[')
              q.enqueue((i + di, j))
            else {
              if (g(i + di)(j) == ']')
                q.enqueue((i + di, j - 1))
              if (g(i + di)(j + 1) == '[')
                q.enqueue((i + di, j + 1))
            }
          }

          if (move) {
            boxes
              .sortBy((i, j) => -di * i)
              .foreach((i, j) => {
                g(i + di)(j) = '['
                g(i + di)(j + 1) = ']'
                g(i)(j) = '.'
                g(i)(j + 1) = '.'
              })

            g(ri)(rj) = '.'
            g(ni)(nj) = '@'
            ri = ni
            rj = nj
          }
        }
      } else if (g(ni)(nj) == '.') {
        g(ri)(rj) = '.'
        g(ni)(nj) = '@'
        ri = ni
        rj = nj
      }

      op = op + 1
    }

    g.map(_.mkString).toVector
  }

  def coordinateSum(grid: Vector[String]): Long =
    grid.zipWithIndex
      .map((s, i) => s.zipWithIndex.map((ch, j) => if (ch == 'O' || ch == '[') 100L * i + j else 0L).sum)
      .sum

  def run(input: String): (Long, Long) = {
    val (grid, ops) = {
      val (grid, opsStr) = input.split("\n").span(_ != "")
      (grid.toVector, opsStr.mkString)
    }

    val part1 = coordinateSum(move(grid, ops))

    val wideGrid = grid.map(s =>
      s.map({
        case '#' => "##"
        case 'O' => "[]"
        case '.' => ".."
        case '@' => "@."
      }).mkString
    )

    val part2 = coordinateSum(wideMove(wideGrid, ops))

    (part1, part2)
  }
}
