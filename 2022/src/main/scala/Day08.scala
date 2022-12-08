import scala.collection.mutable

object Day08 extends DailyChallenge[Int, Int] {
  final val HorizontalAndVerticalDiffs = List((0, 1), (0, -1), (1, 0), (-1, 0))

  def visibleTrees(grid: Vector[Vector[Int]]): Int = {
    val height = grid.length
    val width = grid(0).length
    val diffs = HorizontalAndVerticalDiffs

    def atBorder(i: Int, j: Int): Boolean =
      i == 0 || j == 0 || i + 1 == height || j + 1 == width

    val maxAtCache = mutable.Map.empty[(Int, Int, (Int, Int)), Int]
    def maxAt(i: Int, j: Int, diff: (Int, Int)): Int = {
      if (!maxAtCache.contains((i, j, diff)))
        maxAtCache((i, j, diff)) =
          if (atBorder(i, j)) grid(i)(j)
          else math.max(grid(i)(j), maxAt(i + diff._1, j + diff._2, diff))
      maxAtCache((i, j, diff))
    }

    def isVisible(i: Int, j: Int): Boolean =
      atBorder(i, j) || diffs.exists(diff => grid(i)(j) > maxAt(i + diff._1, j + diff._2, diff))

    (0 until height).map(i => (0 until width).count(j => isVisible(i, j))).sum
  }

  def highestScenicScore(grid: Vector[Vector[Int]]): Int = {
    val height = grid.length
    val width = grid(0).length
    val diffs = HorizontalAndVerticalDiffs

    def maxSeen(i: Int, j: Int, treeHeight: Int, diff: (Int, Int)): Int =
      if (i < 0 || j < 0 || i >= height || j >= width) 0
      else if (grid(i)(j) >= treeHeight) 1
      else 1 + maxSeen(i + diff._1, j + diff._2, treeHeight, diff)

    def scenicScore(i: Int, j: Int): Int =
      diffs.map(diff => maxSeen(i + diff._1, j + diff._2, grid(i)(j), diff)).product

    (0 until height).flatMap(i => (0 until width).map(j => scenicScore(i, j))).max
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.map(_ - '0').toVector).toVector

    val part1 = visibleTrees(grid)
    val part2 = highestScenicScore(grid)

    (part1, part2)
  }
}
