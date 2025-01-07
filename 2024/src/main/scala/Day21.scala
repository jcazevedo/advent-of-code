import scala.collection.mutable

object Day21 extends DailyChallenge[Long, Long] {
  val NumericKeypad = Vector("789", "456", "123", " 0A")
  val DirectionalKeypad = Vector(" ^A", "<v>")
  val Diffs = Map('^' -> (-1, 0), '>' -> (0, 1), 'v' -> (1, 0), '<' -> (0, -1))

  def paths(keypad: Vector[String]): Map[(Char, Char), Set[String]] = {
    val H = keypad.size
    val W = keypad.head.size

    def good(pos: (Int, Int)): Boolean =
      pos._1 >= 0 && pos._1 < H && pos._2 >= 0 && pos._2 < W && keypad(pos._1)(pos._2) != ' '

    def bfs(start: (Int, Int)): Map[(Int, Int), Set[String]] = {
      val q = mutable.Queue.empty[(Int, Int)]
      val best = mutable.Map.empty[(Int, Int), Set[String]].withDefaultValue(Set.empty[String])
      q.enqueue(start)
      best(start) = Set("")

      while (q.nonEmpty) {
        val (i, j) = q.dequeue()
        val currLength = best((i, j)).head.length

        Diffs.foreach({ case (ch, (di, dj)) =>
          val ni = i + di
          val nj = j + dj
          val next = (ni, nj)
          if (good(next) && (best(next).isEmpty || currLength + 1 <= best(next).head.length)) {
            val nextBest = best((i, j)).map(curr => s"$curr$ch")
            if (best(next).isEmpty || currLength + 1 < best(next).head.length) {
              best(next) = nextBest
              q.enqueue(next)
            } else {
              best(next) = best(next) ++ nextBest
            }
          }
        })
      }

      best.map({ case (k, v) => k -> v.map(path => s"${path}A") }).toMap
    }

    (for {
      i <- 0 until H
      j <- 0 until W
      startC = keypad(i)(j)
      if startC != ' '
      (endP, shortestPaths) <- bfs((i, j))
      endC = keypad(endP._1)(endP._2)
    } yield (startC, endC) -> shortestPaths).toMap
  }

  def run(input: String): (Long, Long) = {
    val codes = input.split("\n").toVector

    val numericKeypadPaths = paths(NumericKeypad)
    val directionalKeypadPaths = paths(DirectionalKeypad)
    val cache = mutable.Map.empty[(String, Int, Int), Long]

    def shortestPath(target: String, depth: Int): Long = {
      def shortestPathAux(target: String, currDepth: Int, maxDepth: Int): Long = {
        if (cache.contains((target, currDepth, maxDepth)))
          cache((target, currDepth, maxDepth))
        else {
          val path = s"A$target"
          val paths = if (currDepth == 0) numericKeypadPaths else directionalKeypadPaths
          val best = path
            .zip(path.tail)
            .map((from, to) => {
              if (currDepth == maxDepth)
                paths((from, to)).head.length.toLong
              else
                paths((from, to)).map(shortestPathAux(_, currDepth + 1, maxDepth)).min
            })
            .sum

          cache((target, currDepth, maxDepth)) = best
          best
        }
      }

      shortestPathAux(target, 0, depth)
    }

    val part1 = codes
      .map(code => {
        val pathLength = shortestPath(code, 2)
        val numericCode = code.take(3).toInt
        pathLength * numericCode
      })
      .sum

    val part2 = codes
      .map(code => {
        val pathLength = shortestPath(code, 25)
        val numericCode = code.take(3).toInt
        pathLength * numericCode
      })
      .sum

    (part1, part2)
  }
}
