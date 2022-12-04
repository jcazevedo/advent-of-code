import scala.collection.mutable

object Day12 extends DailyChallenge[Int, Int] {
  case class Graph(adjacencies: Map[String, List[String]])

  object Graph {
    final val empty: Graph = Graph(Map.empty)
  }

  case class Path(path: Vector[String]) {
    lazy val visitedSmallCaves: Map[String, Int] =
      path.filter(_.head.isLower).groupBy(identity).view.mapValues(_.size).toMap
  }

  def getPathsPart1(graph: Graph): Set[Path] = {
    val res = mutable.Set.empty[Path]
    val q = mutable.Queue.empty[Path]
    q.enqueue(Path(Vector("start")))

    while (q.nonEmpty) {
      val currPath = q.dequeue()
      val lastCave = currPath.path.last

      graph.adjacencies.getOrElse(lastCave, List.empty).foreach { nextCave =>
        if (!currPath.visitedSmallCaves.contains(nextCave)) {
          val nextPath = currPath.copy(path = currPath.path :+ nextCave)
          if (nextCave == "end") res.add(nextPath)
          else q.enqueue(nextPath)
        }
      }
    }

    res.toSet
  }

  def getPathsPart2(graph: Graph): Set[Path] = {
    val res = mutable.Set.empty[Path]
    val q = mutable.Queue.empty[Path]
    q.enqueue(Path(Vector("start")))

    while (q.nonEmpty) {
      val currPath = q.dequeue()
      val lastCave = currPath.path.last

      graph.adjacencies.getOrElse(lastCave, List.empty).foreach { nextCave =>
        if (
          nextCave != "start" && currPath.visitedSmallCaves.values.count(_ > 1) == 0 || !currPath.visitedSmallCaves
            .contains(nextCave)
        ) {
          val nextPath = currPath.copy(path = currPath.path :+ nextCave)
          if (nextCave == "end") res.add(nextPath)
          else q.enqueue(nextPath)
        }
      }
    }

    res.toSet
  }

  def run(input: String): (Int, Int) = {
    val graph = input
      .split("\n")
      .foldLeft(Graph.empty)((graph, line) => {
        val Array(from, to) = line.split("-")
        Graph(
          graph.adjacencies
            .updatedWith(from)({
              case None    => Some(List(to))
              case Some(l) => Some(to :: l)
            })
            .updatedWith(to)({
              case None    => Some(List(from))
              case Some(l) => Some(from :: l)
            })
        )
      })

    val part1 = getPathsPart1(graph).size
    val part2 = getPathsPart2(graph).size

    (part1, part2)
  }
}
