package net.jcazevedo.adventofcode

class Day19 extends DailyChallenge[String, Int] {
  def run(filename: String): (String, Int) = {
    val maze = io.Source.fromFile(filename).getLines().toIndexedSeq

    def start(maze: IndexedSeq[String]): (Int, Int) =
      (0, maze.head.indexOf('|'))

    def visit(prevPos: (Int, Int), currentPos: (Int, Int), maze: IndexedSeq[String], currentString: String = "", nSteps: Int = 0): (String, Int) = {
      val i = currentPos._1
      val j = currentPos._2
      val di = i - prevPos._1
      val dj = j - prevPos._2
      if (i < 0 || i >= maze.length || j < 0 || j >= maze(i).length || maze(i)(j) == ' ')
        (currentString, nSteps)
      else if (maze(i)(j) == '|' || maze(i)(j) == '-')
        visit(currentPos, (i + di, j + dj), maze, currentString, nSteps + 1)
      else if (maze(i)(j) == '+') {
        val neighbors = List((0, 1), (1, 0), (0, -1), (-1, 0))
        val next = neighbors.find {
          case (di, dj) =>
            i + di != prevPos._1 &&
              j + dj != prevPos._2 &&
              i + di >= 0 &&
              i + di < maze.length &&
              j + dj >= 0 &&
              j + dj < maze(i + di).length &&
              maze(i + di)(j + dj) != ' '
        }
        next match {
          case Some((di, dj)) =>
            visit(currentPos, (i + di, j + dj), maze, currentString, nSteps + 1)
          case None =>
            (currentString, nSteps)
        }
      } else {
        visit(currentPos, (i + di, j + dj), maze, currentString + maze(i)(j), nSteps + 1)
      }
    }

    val s = start(maze)
    visit((s._1 - 1, s._2), s, maze)
  }
}
