object Day04 extends DailyChallenge[Int, Int] {
  case class Board(grid: Vector[Vector[Int]]) {
    def wins = grid.exists(_.forall(_ == -1)) || {
      val length = grid(0).length
      (0 until length).exists(col => grid.forall(_(col) == -1))
    }

    def mark(num: Int): Board =
      if (!grid.exists(_.contains(num))) this
      else {
        copy(grid = grid.zipWithIndex.foldLeft(grid)({ case (nextGrid, (row, rowIdx)) =>
          nextGrid.updated(
            rowIdx,
            row.zipWithIndex.foldLeft(row)({
              case (nextRow, (x, colIdx)) if x == num => nextRow.updated(colIdx, -1)
              case (nextRow, (_, _))                  => nextRow
            })
          )
        }))
      }

    def sumUnmarked: Int =
      grid.map(_.filter(_ >= 0).sum).sum
  }

  def firstWinner(draws: List[Int], boards: Vector[Board]): (Int, Board) =
    draws match {
      case h :: t =>
        val nextBoards = boards.map(_.mark(h))
        nextBoards.find(_.wins) match {
          case None        => firstWinner(t, nextBoards)
          case Some(value) => (h, value)
        }

      case Nil => (-1, Board(Vector.empty)) // This shouldn't happen
    }

  def lastWinner(draws: List[Int], boards: Vector[Board]): (Int, Board) =
    draws match {
      case h :: t =>
        val nextBoards = boards.map(_.mark(h)).filterNot(_.wins)
        if (nextBoards.isEmpty) (h, boards.head.mark(h))
        else lastWinner(t, nextBoards)

      case Nil => (-1, Board(Vector.empty)) // This shouldn't happen
    }

  def run(input: String): (Int, Int) = {
    val lines = input.split("\n").toList

    val draws = lines.head.split(",").map(_.toInt).toList

    val boards = lines.tail
      .sliding(6, 6)
      .map(boardLines => Board(boardLines.tail.map(_.trim.split("\\s+").map(_.toInt).toVector).toVector))
      .toVector

    val (lastDraw1, winner1) = firstWinner(draws, boards)
    val part1 = lastDraw1 * winner1.sumUnmarked

    val (lastDraw2, winner2) = lastWinner(draws, boards)
    val part2 = lastDraw2 * winner2.sumUnmarked

    (part1, part2)
  }
}
