import scala.collection.mutable

object Day23 extends DailyChallenge[Int, String] {
  def getAllConnected(graph: Map[String, Set[String]]): Set[Set[String]] = {
    val ans = mutable.Set.empty[Set[String]]
    val visited = mutable.Set.empty[Set[String]]

    def go(inSet: Set[String], remaining: Set[String]): Unit = {
      ans.addOne(inSet)
      val options = remaining.filter(next => graph(next).intersect(inSet) == inSet)
      val toKeep = remaining.intersect(options)
      toKeep.foreach(next => {
        if (!visited(inSet + next)) {
          visited.addOne(inSet + next)
          go(inSet + next, toKeep - next)
        }
      })
    }

    val nodes = graph.keySet
    graph.keySet.foreach(node => go(Set(node), nodes - node))

    ans.toSet
  }

  def run(input: String): (Int, String) = {
    val graph = {
      val tmp = mutable.Map.empty[String, Set[String]]
      input
        .split("\n")
        .foreach(line => {
          val Array(u, v) = line.split("-")
          tmp.updateWith(u)({
            case None       => Some(Set(v))
            case Some(curr) => Some(curr + v)
          })
          tmp.updateWith(v)({
            case None       => Some(Set(u))
            case Some(curr) => Some(curr + u)
          })
        })
      tmp.toMap
    }

    val part1 =
      getAllConnected(graph).filter(_.size == 3).filter(_.exists(_.startsWith("t"))).size
    val part2 =
      getAllConnected(graph).maxBy(_.size).toList.sorted.mkString(",")

    (part1, part2)
  }
}
