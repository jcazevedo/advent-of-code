import scala.collection.mutable

object Day18 extends DailyChallenge[Int, String] {
  def shortestPath(corrupted: List[(Int, Int)], width: Int, height: Int): Int = {
    val isCorrupted = corrupted.toSet

    val q = mutable.Queue.empty[(Int, Int)]
    val dist = mutable.Map.empty[(Int, Int), Int]

    q.enqueue((0, 0))
    dist((0, 0)) = 0

    def good(pos: (Int, Int)): Boolean =
      pos._1 >= 0 && pos._1 <= width && pos._2 >= 0 && pos._2 <= height && !isCorrupted(pos) && !dist.contains(pos)

    while (q.nonEmpty) {
      val (x, y) = q.dequeue()

      (-1 to 1).foreach(dx =>
        (-1 to 1).foreach(dy =>
          if (math.abs(dx) + math.abs(dy) == 1) {
            val nx = x + dx
            val ny = y + dy
            if (good((nx, ny))) {
              dist((nx, ny)) = dist((x, y)) + 1
              q.enqueue((nx, ny))
            }
          }
        )
      )
    }

    dist.getOrElse((width, height), -1)
  }

  def run(input: String): (Int, String) = {
    val bytes = input
      .split("\n")
      .map(line => {
        val Array(x, y) = line.split(",")
        (x.toInt, y.toInt)
      })
      .toList

    val part1 = shortestPath(bytes.take(1024), 70, 70)
    val part2 = bytes.zipWithIndex
      .find({ case ((x, y), idx) => shortestPath(bytes.take(idx + 1), 70, 70) == -1 })
      .map({ case ((x, y), _) => s"$x,$y" })
      .getOrElse("-1")

    (part1, part2)
  }
}
