import scala.collection.mutable

object Day15 extends DailyChallenge[Int, Int] {
  def getLowestCostPath(grid: Vector[Vector[Int]], rep: Int): Int = {
    val pq = mutable.PriorityQueue.empty[(Int, (Int, Int))]
    val bestAt = mutable.Map.empty[(Int, Int), Int]

    val height = grid.length
    val width = grid(0).length

    def costAt(h: Int, w: Int): Int = {
      val initH = h % grid.length
      val initW = w % grid(0).length
      val additional = h / height + w / width
      ((grid(initH)(initW) + additional - 1) % 9) + 1
    }

    val start = (0, 0)
    val finish = (height * rep - 1, width * rep - 1)

    bestAt(start) = 0
    pq.enqueue((0, start))

    while (pq.nonEmpty) {
      val (currDistNeg, (currH, currW)) = pq.dequeue()
      val currDist = -currDistNeg

      if (
        currH > 0 && (!bestAt
          .contains((currH - 1, currW)) || currDist + costAt(currH - 1, currW) < bestAt((currH - 1, currW)))
      ) {
        val nextDist = currDist + costAt(currH - 1, currW)
        bestAt((currH - 1, currW)) = nextDist
        pq.enqueue((-nextDist, (currH - 1, currW)))
      }

      if (
        currH + 1 < height * rep && (!bestAt
          .contains((currH + 1, currW)) || currDist + costAt(currH + 1, currW) < bestAt((currH + 1, currW)))
      ) {
        val nextDist = currDist + costAt(currH + 1, currW)
        bestAt((currH + 1, currW)) = nextDist
        pq.enqueue((-nextDist, (currH + 1, currW)))
      }

      if (
        currW > 0 && (!bestAt
          .contains((currH, currW - 1)) || currDist + costAt(currH, currW - 1) < bestAt((currH, currW - 1)))
      ) {
        val nextDist = currDist + costAt(currH, currW - 1)
        bestAt((currH, currW - 1)) = nextDist
        pq.enqueue((-nextDist, (currH, currW - 1)))
      }

      if (
        currW + 1 < height * rep && (!bestAt
          .contains((currH, currW + 1)) || currDist + costAt(currH, currW + 1) < bestAt((currH, currW + 1)))
      ) {
        val nextDist = currDist + costAt(currH, currW + 1)
        bestAt((currH, currW + 1)) = nextDist
        pq.enqueue((-nextDist, (currH, currW + 1)))
      }
    }

    bestAt(finish)
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.map(_ - '0').toVector).toVector

    val part1 = getLowestCostPath(grid, 1)
    val part2 = getLowestCostPath(grid, 5)

    (part1, part2)
  }
}
