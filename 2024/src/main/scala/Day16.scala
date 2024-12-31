import scala.collection.mutable

object Day16 extends DailyChallenge[Long, Int] {
  final val Dirs: List[(Int, Int)] =
    List((0, 1), (1, 0), (0, -1), (-1, 0))
  final val Clockwise: Map[(Int, Int), (Int, Int)] =
    Dirs.zip(Dirs.tail :+ Dirs.head).toMap
  final val CounterClockwise: Map[(Int, Int), (Int, Int)] =
    Dirs.zip(Dirs.last :: Dirs.init).toMap

  case class State(i: Int, j: Int, dir: (Int, Int))

  def bestScore(grid: Vector[String]): (Long, Int) = {
    val H = grid.length
    val W = grid(0).length

    var si = -1
    var sj = -1
    var ei = -1
    var ej = -1
    (0 until H).foreach(i =>
      (0 until W).foreach(j => {
        if (grid(i)(j) == 'S') {
          si = i
          sj = j
        }
        if (grid(i)(j) == 'E') {
          ei = i
          ej = j
        }
      })
    )

    val stateCost = mutable.Map.empty[State, Long].withDefaultValue(Long.MaxValue)
    val posCost = mutable.Map.empty[(Int, Int), Long].withDefaultValue(Long.MaxValue)
    val prev = mutable.Map.empty[State, mutable.Set[State]]

    val pq = mutable.PriorityQueue.apply[(Long, State)]()(Ordering.by(-_._1))

    val start = State(si, sj, (0, 1))
    stateCost(start) = 0L
    posCost((si, sj)) = 0L
    pq.enqueue((0L, start))

    while (pq.nonEmpty) {
      val (dist, curr @ State(i, j, dir @ (di, dj))) = pq.dequeue()

      val nextForward = State(i + di, j + dj, dir)
      if (grid(i + di)(j + dj) != '#' && dist + 1L <= stateCost(nextForward)) {
        if (dist + 1L == stateCost(nextForward)) {
          prev(nextForward).addOne(curr)
        }

        if (dist + 1L < stateCost(nextForward)) {
          if (prev.contains(nextForward)) prev(nextForward).clear()
          else prev(nextForward) = mutable.Set.empty[State]
          prev(nextForward).addOne(curr)
          stateCost(nextForward) = dist + 1L
          posCost((i + di, j + dj)) = math.min(posCost((i + di, j + dj)), dist + 1L)
          pq.enqueue((dist + 1, nextForward))
        }
      }

      val nextCW = State(i, j, Clockwise(dir))
      if (dist + 1000L == stateCost(nextCW)) {
        prev(nextCW).addOne(curr)
      }
      if (dist + 1000L < stateCost(nextCW)) {
        if (prev.contains(nextCW)) prev(nextCW).clear()
        else prev(nextCW) = mutable.Set.empty[State]
        prev(nextCW).addOne(curr)
        stateCost(nextCW) = dist + 1000L
        pq.enqueue((dist + 1000L, nextCW))
      }

      val nextCCW = State(i, j, CounterClockwise(dir))
      if (dist + 1000L == stateCost(nextCCW)) {
        prev(nextCCW).addOne(curr)
      }
      if (dist + 1000L < stateCost(nextCCW)) {
        if (prev.contains(nextCCW)) prev(nextCCW).clear()
        else prev(nextCCW) = mutable.Set.empty[State]
        prev(nextCCW).addOne(curr)
        stateCost(nextCCW) = dist + 1000L
        pq.enqueue((dist + 1000L, nextCCW))
      }
    }

    val inBestPaths = mutable.Set.empty[(Int, Int)]
    val q = mutable.Queue.empty[State]
    Dirs.foreach(dir => {
      val state = State(ei, ej, dir)
      if (stateCost(state) == posCost((ei, ej))) q.enqueue(state)
    })
    while (q.nonEmpty) {
      val curr @ State(i, j, _) = q.dequeue()
      inBestPaths.addOne((i, j))
      if (prev.contains(curr)) prev(curr).foreach(q.enqueue)
    }

    (posCost((ei, ej)), inBestPaths.size)
  }

  def run(input: String): (Long, Int) = {
    val grid = input.split("\n").toVector
    val (part1, part2) = bestScore(grid)
    (part1, part2)
  }
}
