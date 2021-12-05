package net.jcazevedo.adventofcode

class Day18 extends DailyChallenge[Int, Int] {
  def nextTile(prevThree: String): Char = {
    if (prevThree == "^^." || prevThree == ".^^" || prevThree == "^.." || prevThree == "..^")
      '^'
    else
      '.'
  }

  def nextRow(prevRow: String): String = {
    ('.' + prevRow + '.').sliding(3).map(nextTile).mkString
  }

  def run(filename: String): (Int, Int) = {
    val firstRow = io.Source.fromFile(filename).getLines.mkString
    val rows1 = (0 until 39).foldLeft(List(firstRow)) {
      case (rows, _) =>
        nextRow(rows.head) :: rows
    }
    val (_, safeCount) = (0 until 399999).foldLeft((firstRow, firstRow.count(_ == '.'))) {
      case ((row, count), _) =>
        val next = nextRow(row)
        (next, count + next.count(_ == '.'))
    }
    (rows1.map(_.count(_ == '.')).sum, safeCount)
  }
}
