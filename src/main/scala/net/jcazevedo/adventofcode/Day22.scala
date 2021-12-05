package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day22 extends DailyChallenge[Int, Int] {
  case class Node(fileSystem: String, size: Int, used: Int, x: Int, y: Int) {
    lazy val avail: Int = size - used
  }

  type State = (Map[(Int, Int), Node], (Int, Int), (Int, Int))

  def go(nodes: Seq[Node]): Int = {
    val startM = nodes.map { node =>
      (node.x, node.y) -> node
    }.toMap
    val maxX = startM.keys.maxBy(_._1)
    val empty = startM.filter(_._2.used == 0).head._1
    val start = (startM, empty, maxX)
    val targetData = startM(maxX).used

    def heuristic(state: State) = {
      state._3._1 + state._3._2 + math.abs(state._2._1 - state._3._1) + math.abs(state._2._2 - state._3._2)
    }

    implicit val ordering: Ordering[(Int, Int, State)] =
      Ordering.by[(Int, Int, State), Int] {
        case (dist, heur, state) =>
          dist + heur
      }.reverse

    val dirs = Seq((1, 0), (-1, 0), (0, 1), (0, -1))
    val visited = mutable.Set[((Int, Int), (Int, Int))]((start._2, start._3))
    val pq = mutable.PriorityQueue[(Int, Int, State)]()
    pq += ((0, heuristic(start), start))

    while (!pq.isEmpty) {
      val (dist, _, (nodes, empty, target)) = pq.dequeue()

      if (target == ((0, 0)))
        return dist

      dirs.foreach {
        case (dx, dy) =>
          val otherEmpty = (empty._1 + dx, empty._2 + dy)
          if (nodes.contains(otherEmpty) && nodes(otherEmpty).used <= nodes(empty).size) {
            val nextState = (
              nodes
              .updated(otherEmpty, nodes(otherEmpty).copy(used = 0))
              .updated(empty, nodes(empty).copy(used = nodes(otherEmpty).used)),
              otherEmpty,
              if (otherEmpty == target) empty else target)

            if (!visited.contains((nextState._2, nextState._3))) {
              pq += ((dist + 1, heuristic(nextState), nextState))
              visited += ((nextState._2, nextState._3))
            }
          }
      }
    }

    -1
  }

  def run(filename: String): (Int, Int) = {
    val nodes = io.Source.fromFile(filename).getLines.toList.drop(2).map { s =>
      val ss = s.split("\\s+")
      val id = ss(0)
      val ids = id.drop("/dev/grid/node-".length).split("-")
      Node(
        id,
        ss(1).dropRight(1).toInt,
        ss(2).dropRight(1).toInt,
        ids(0).drop(1).toInt,
        ids(1).drop(1).toInt)
    }
    val viableNodes = for {
      n1 <- nodes
      n2 <- nodes
      if (n1.fileSystem != n2.fileSystem)
      if (n1.used != 0)
      if (n2.avail >= n1.used)
    } yield (n1, n2)
    (viableNodes.size, go(nodes))
  }
}
