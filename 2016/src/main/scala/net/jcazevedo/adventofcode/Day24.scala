package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day24 extends DailyChallenge[Int, Int] {
  type State = ((Int, Int), Int)

  def go(start: (Int, Int), targets: Map[Char, (Int, Int)], map: IndexedSeq[String], endAtStart: Boolean): Int = {
    val tts = targets.values.toIndexedSeq
    val target = (1 << tts.size) - 1
    val dirs = Seq((-1, 0), (1, 0), (0, 1), (0, -1))

    implicit val ordering: Ordering[(Int, State)] =
      Ordering.by[(Int, State), Int] {
        case (dist, _) => dist
      }.reverse

    val startState = (start, 0)
    val visited = mutable.Set[State](startState)
    val pq = mutable.PriorityQueue[(Int, State)]()
    val W = map(0).size
    val H = map.size
    pq += ((0, startState))

    while (!pq.isEmpty) {
      val (dist, ((x, y), collected)) = pq.dequeue()

      if (collected == target) {
        if (!endAtStart) {
          return dist
        } else if ((x, y) == start)
          return dist
      }

      dirs.foreach {
        case (dx, dy) =>
          val (nx, ny) = (x + dx, y + dy)
          if (nx >= 0 && nx < W && ny >= 0 && ny < H && map(ny)(nx) != '#') {
            val idx = tts.indexOf((nx, ny))
            val nextCollected = if (idx == -1)
              collected
            else
              (collected | (1 << idx))
            val nextState = ((nx, ny), nextCollected)

            if (!visited.contains(nextState)) {
              pq += ((dist + 1, nextState))
              visited += nextState
            }
          }
      }
    }

    -1
  }

  def run(filename: String): (Int, Int) = {
    val preMap = io.Source.fromFile(filename).getLines.toIndexedSeq
    val (finalMap, targets) = preMap.zipWithIndex.foldLeft((preMap, Map[Char, (Int, Int)]())) {
      case ((map, targets), (line, y)) =>
        val (finalLine, tt) = line.zipWithIndex.foldLeft(("", targets)) {
          case ((line, targets), (ch, x)) =>
            if (ch != '.' && ch != '#')
              (line + '.', targets.updated(ch, (x, y)))
            else
              (line + ch, targets)
        }
        (map.updated(y, finalLine), tt)
    }
    (go(targets('0'), targets - '0', finalMap, false), go(targets('0'), targets - '0', finalMap, true))
  }
}
