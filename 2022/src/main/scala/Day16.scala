import scala.collection.mutable

object Day16 extends DailyChallenge[Int, Int] {
  case class Valve(flowRate: Int, tunnels: List[String])

  def distances(graph: Map[String, Valve]): Map[(String, String), Int] = {
    val dists = mutable.Map.empty[(String, String), Int]

    val nodes = graph.keySet
    for {
      n1 <- nodes
      n2 <- nodes
      dist =
        if (n1 == n2) 0
        else if (graph(n1).tunnels.contains(n2)) 1
        else -1
    } dists((n1, n2)) = dist

    for {
      k <- nodes
      i <- nodes
      if dists((i, k)) != -1
      j <- nodes
      if dists((k, j)) != -1
      sum = dists((i, k)) + dists((k, j))
      if dists((i, j)) == -1 || sum < dists((i, j))
    } dists((i, j)) = sum

    dists.toMap
  }

  def mostPressure(graph: Map[String, Valve], maxMinutes: Int): Int = {
    // We're relying on the fact that for N valves with flow rate > 0, 2^N is small enough.
    case class State(currentValve: String, minutes: Int, valvesOn: Int, score: Int)

    val valvesIdx = graph.filter(_._2.flowRate > 0).map(_._1).zipWithIndex.toMap
    val dists = distances(graph)

    val best = mutable.Map.empty[(Int, Int), Int]
    val q = mutable.Queue.empty[State]
    val start = State("AA", 0, 0, 0)
    q.enqueue(start)
    best((0, 0)) = 0

    while (q.nonEmpty) {
      val curr @ State(currentValve, minutes, valvesOn, currScore) = q.dequeue()

      valvesIdx.foreach { case (v, idx) =>
        if ((valvesOn & (1 << idx)) == 0 && dists((currentValve, v)) != -1) {
          val nextMinutes = minutes + dists((currentValve, v)) + 1
          if (nextMinutes < maxMinutes) {
            val nextScore = currScore + (maxMinutes - nextMinutes) * graph(v).flowRate
            val nextState = State(v, nextMinutes, valvesOn | (1 << idx), nextScore)
            if (
              !best.contains((nextState.valvesOn, nextMinutes)) || best((nextState.valvesOn, nextMinutes)) < nextScore
            ) {
              best((nextState.valvesOn, nextMinutes)) = nextScore
              q.enqueue(nextState)
            }
          }
        }
      }
    }

    best.values.max
  }

  def mostPressureWithHelp(graph: Map[String, Valve], maxMinutes: Int): Int = {
    // We're relying on the fact that for N valves with flow rate > 0, 2^N is small enough.
    case class State(
        currentValve1: String,
        minutes1: Int,
        currentValve2: String,
        minutes2: Int,
        valvesOn: Int,
        score: Int
    )

    val valvesIdx = graph.filter(_._2.flowRate > 0).map(_._1).zipWithIndex.toMap
    val dists = distances(graph)

    val best = mutable.Map.empty[(Int, Int, Int), Int]
    val q = mutable.Queue.empty[State]
    val start = State("AA", 0, "AA", 0, 0, 0)
    q.enqueue(start)
    best((0, 0, 0)) = 0

    def getNext(valve: String, minutes: Int, valvesOn: Int): List[(String, Int)] =
      (valve, minutes) :: valvesIdx.toList.flatMap { case (nv, i) =>
        val d = dists((valve, nv))
        val nm = minutes + d + 1
        Option.when(nm < maxMinutes && (valvesOn & (1 << i)) == 0 && d != -1)((nv, nm))
      }

    while (q.nonEmpty) {
      val curr @ State(v1, m1, v2, m2, valvesOn, currScore) = q.dequeue()

      for {
        (nv1, nm1) <- getNext(v1, m1, valvesOn)
        (nv2, nm2) <- getNext(v2, m2, valvesOn)
        if nv2 != nv1
        nv1Score =
          if (valvesIdx.contains(nv1) && (valvesOn & (1 << valvesIdx(nv1))) == 0)
            (maxMinutes - nm1) * graph(nv1).flowRate
          else 0
        nv2Score =
          if (valvesIdx.contains(nv2) && (valvesOn & (1 << valvesIdx(nv2))) == 0)
            (maxMinutes - nm2) * graph(nv2).flowRate
          else 0
        nextScore = currScore + nv1Score + nv2Score
        nextValvesOn = valvesOn |
          (if (graph(nv1).flowRate > 0) (1 << valvesIdx(nv1)) else 0) |
          (if (graph(nv2).flowRate > 0) (1 << valvesIdx(nv2)) else 0)
        nextState =
          if (nv1 < nv2) State(nv1, nm1, nv2, nm2, nextValvesOn, nextScore)
          else State(nv2, nm2, nv1, nm1, nextValvesOn, nextScore)
        if !best.contains((nextState.valvesOn, nextState.minutes1, nextState.minutes2)) || best(
          (nextState.valvesOn, nextState.minutes1, nextState.minutes2)
        ) < nextScore
      } {
        best((nextState.valvesOn, nextState.minutes1, nextState.minutes2)) = nextScore
        q.enqueue(nextState)
      }
    }

    best.values.max
  }

  def run(input: String): (Int, Int) = {
    val valveRegex = "^Valve ([^\\s]+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)$".r

    val graph = input
      .split("\n")
      .map(_.trim)
      .foldLeft(Map.empty[String, Valve]) { (curr, line) =>
        val Some(List(valve, rate, connections)) = (valveRegex.unapplySeq(line): @unchecked)
        curr.updated(valve, Valve(rate.toInt, connections.split(", ").toList))
      }

    val part1 = mostPressure(graph, 30)
    val part2 = mostPressureWithHelp(graph, 26)

    (part1, part2)
  }
}
