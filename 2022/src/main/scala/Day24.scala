import scala.collection.mutable
import scala.util.control.NonLocalReturns._

object Day24 extends DailyChallenge[Int, Int] {
  final val NextMoves: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0), (0, 0))
  final val BlizzardDir: Map[Char, (Int, Int)] = Map('>' -> (0, 1), '<' -> (0, -1), '^' -> (-1, 0), 'v' -> (1, 0))

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def minMoves(grid: Vector[String], targetState: Int): Int = {
    val height = grid.length
    val width = grid(0).length

    val start = (0, grid(0).indexOf('.'))
    val finish = (height - 1, grid(height - 1).indexOf('.'))

    val blizzards = grid.zipWithIndex.flatMap { case (line, i) =>
      line.zipWithIndex.flatMap {
        case (ch, j) if ch != '.' && ch != '#' => Some((i, j) -> ch)
        case _                                 => None
      }
    }.toMap

    val blizzardsCache = mutable.Map.empty[Int, Set[(Int, Int)]]

    def moveBlizzards(minutes: Int): Set[(Int, Int)] = {
      val idx = minutes % lcm(height - 2, width - 2)
      if (!blizzardsCache.contains(idx)) {
        blizzardsCache(idx) = blizzards.toList.map { case ((i, j), ch) =>
          val (di, dj) = BlizzardDir(ch)
          (1 + Math.floorMod(i - 1 + di * minutes, height - 2), 1 + Math.floorMod(j - 1 + dj * minutes, width - 2))
        }.toSet
      }
      blizzardsCache(idx)
    }

    val q = mutable.Queue.empty[(Int, (Int, Int), Int)]
    val visited = mutable.Set.empty[(Int, (Int, Int), Int)]
    val startState = (0, start, 0)
    q.enqueue(startState)
    visited.add(startState)

    returning {
      while (q.nonEmpty) {
        val (minutes, (i, j), state) = q.dequeue()

        val blizzards = moveBlizzards(minutes + 1)

        NextMoves.foreach { case (di, dj) =>
          val ni = i + di
          val nj = j + dj
          if ((ni, nj) == finish && state + 1 == targetState)
            throwReturn(minutes + 1)

          val nextState = (
            minutes + 1,
            (ni, nj),
            if (state % 2 == 0 && (ni, nj) == finish) state + 1
            else if (state % 2 == 1 && (ni, nj) == start) state + 1
            else state
          )
          if (
            ni >= 0 && ni < height && nj >= 0 && nj < width && grid(ni)(nj) != '#' && !blizzards.contains(
              (ni, nj)
            ) && !visited(nextState)
          ) {
            visited.add(nextState)
            q.enqueue(nextState)
          }
        }
      }
      -1
    }
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.trim).toVector

    val part1 = minMoves(grid, targetState = 1)
    val part2 = minMoves(grid, targetState = 3)

    (part1, part2)
  }
}
