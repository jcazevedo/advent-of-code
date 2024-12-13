import scala.collection.mutable

object Day12 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.toVector).toVector
    val Rows = grid.length
    val Cols = grid(0).length
    val Diffs = Vector((-1, 0), (0, 1), (1, 0), (0, -1))

    def inside(pos: (Int, Int)): Boolean =
      pos._1 >= 0 && pos._1 < Rows && pos._2 >= 0 && pos._2 < Cols

    def add(pos: (Int, Int), diff: (Int, Int)): (Int, Int) =
      (pos._1 + diff._1, pos._2 + diff._2)

    def value(pos: (Int, Int)): Char =
      grid(pos._1)(pos._2)

    def neighbors(pos: (Int, Int)): Vector[(Int, Int)] =
      Diffs
        .flatMap(diff => {
          val next = add(pos, diff)
          if (inside(next) && value(next) == value(pos)) Some(next)
          else None
        })

    val values = {
      val visited = mutable.Set.empty[(Int, Int)]

      def floodFill(pos: (Int, Int)): (Set[(Int, Int)], Int) = {
        visited.addOne(pos)
        val perimeter = 4 - neighbors(pos).size
        val (cells, perimeters) = neighbors(pos)
          .map(next =>
            if (inside(next) && !visited(next) && value(next) == value(pos)) floodFill(next)
            else (Set.empty, 0)
          )
          .unzip

        (cells.flatten.toSet + pos, perimeters.sum + perimeter)
      }

      val neighborChecks = Map((0, 1) -> Set((1, 0), (-1, 0)), (1, 0) -> Set((0, -1), (0, 1)))

      def sides(cells: Set[(Int, Int)]): Int = {
        val total = 4 * cells.size
        val insides = cells.toList.map(cell => neighbors(cell).size).sum
        val repeatedRightDown = cells.count(cell => {
          val right = add(cell, (0, 1))
          val down = add(cell, (1, 0))
          val downRight = add(right, (1, 0))
          inside(right) && value(right) == value(cell) &&
          (!inside(down) || value(down) != value(cell)) &&
          (!inside(downRight) || value(downRight) != value(cell))
        })
        val repeatedRightUp = cells.count(cell => {
          val right = add(cell, (0, 1))
          val up = add(cell, (-1, 0))
          val upRight = add(right, (-1, 0))
          inside(right) && value(right) == value(cell) &&
          (!inside(up) || value(up) != value(cell)) &&
          (!inside(upRight) || value(upRight) != value(cell))
        })
        val repeatedDownLeft = cells.count(cell => {
          val down = add(cell, (1, 0))
          val left = add(cell, (0, -1))
          val downLeft = add(down, (0, -1))
          inside(down) && value(down) == value(cell) &&
          (!inside(left) || value(left) != value(cell)) &&
          (!inside(downLeft) || value(downLeft) != value(cell))
        })
        val repeatedDownRight = cells.count(cell => {
          val down = add(cell, (1, 0))
          val right = add(cell, (0, 1))
          val downRight = add(down, (0, 1))
          inside(down) && value(down) == value(cell) &&
          (!inside(right) || value(right) != value(cell)) &&
          (!inside(downRight) || value(downRight) != value(cell))
        })

        total - insides - repeatedRightDown - repeatedRightUp - repeatedDownLeft - repeatedDownRight
      }

      (0 until Rows).flatMap(i =>
        (0 until Cols).map(j =>
          if (visited((i, j))) (0, 0, 0)
          else {
            val (cells, perimeter) = floodFill((i, j))
            (cells.size, perimeter, sides(cells))
          }
        )
      )
    }

    val part1 = values.map((area, perimeter, _) => area * perimeter).sum
    val part2 = values.map((area, _, sides) => area * sides).sum

    (part1, part2)
  }
}
