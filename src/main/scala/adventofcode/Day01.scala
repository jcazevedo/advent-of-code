package adventofcode

class Day01 extends DailyChallenge[Int, Int] {
  def part1(entries: List[Int]): Int = {
    entries
      .foldLeft[(Set[Int], Option[Int])]((Set.empty, None)) { case (acc @ (visited, ans), curr) =>
        ans match {
          case None if visited(2020 - curr) =>
            (visited, Some(curr * (2020 - curr)))
          case None =>
            (visited + curr, None)
          case _ =>
            acc
        }
      }
      ._2
      .get
  }

  def part2(entries: List[Int]): Int = {
    entries
      .foldLeft[(Set[Int], Map[Int, (Int, Int)], Option[Int])]((Set.empty, Map.empty, None)) {
        case (acc @ (visited, sum2, ans), curr) =>
          ans match {
            case None if sum2.contains(2020 - curr) =>
              val (v1, v2) = sum2(2020 - curr)
              (visited, sum2, Some(v1 * v2 * curr))
            case None =>
              val nextVisited = visited + curr
              val nextSum2 = visited.foldLeft(sum2) { case (currSum, v) =>
                currSum.updated(v + curr, (v, curr))
              }
              (nextVisited, nextSum2, None)
            case _ =>
              acc
          }
      }
      ._3
      .get
  }

  def run(input: String): (Int, Int) = {
    val entries = input.split("\n").map(_.toInt).toList
    (part1(entries), part2(entries))
  }
}
