import scala.collection.mutable

object Day06 extends DailyChallenge[Int, Int] {
  final val Directions = Map(
    '^' -> (-1, 0),
    '>' -> (0, 1),
    'v' -> (1, 0),
    '<' -> (0, -1)
  )

  final val Next = Map(
    '^' -> '>',
    '>' -> 'v',
    'v' -> '<',
    '<' -> '^'
  )

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.toArray)

    val Rows = grid.length
    val Cols = grid(0).length

    def inside(pos: (Int, Int)): Boolean =
      pos._1 >= 0 && pos._1 < Rows && pos._2 >= 0 && pos._2 < Cols

    def obstacle(pos: (Int, Int)): Boolean =
      grid(pos._1)(pos._2) == '#'

    val (initialPos, initialDir) = (for {
      i <- 0 until Rows
      j <- 0 until Cols
      ch = grid(i)(j)
      if ch != '.'
      if ch != '#'
    } yield ((i, j), ch)).head

    val visitedPositions = mutable.Set.empty[(Int, Int)]
    var pos = initialPos
    var dir = initialDir

    while (inside(pos)) {
      visitedPositions.add(pos)
      val currDir = Directions(dir)
      val nextPos = (pos._1 + currDir._1, pos._2 + currDir._2)
      if (inside(nextPos) && obstacle(nextPos))
        dir = Next(dir)
      else
        pos = nextPos
    }

    val part1 = visitedPositions.size

    val part2 = visitedPositions
      .filterNot(_ == initialPos)
      .count(candidate => {
        grid(candidate._1)(candidate._2) = '#'
        val visitedStates = mutable.Set.empty[((Int, Int), Char)]

        var pos = initialPos
        var dir = initialDir
        var loop = false

        while (inside(pos) && !loop) {
          if (visitedStates.contains((pos, dir))) {
            loop = true
          } else {
            visitedStates.add((pos, dir))
            val currDir = Directions(dir)
            val nextPos = (pos._1 + currDir._1, pos._2 + currDir._2)
            if (inside(nextPos) && obstacle(nextPos))
              dir = Next(dir)
            else
              pos = nextPos
          }
        }

        grid(candidate._1)(candidate._2) = '.'

        loop
      })

    (part1, part2)
  }
}
