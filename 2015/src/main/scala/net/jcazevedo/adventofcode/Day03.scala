package net.jcazevedo.adventofcode

class Day03 extends DailyChallenge[Int, Int] {
  case class Pos(x: Int, y: Int) {
    def next(c: Char) = c match {
      case '^' => this.copy(y = this.y + 1)
      case 'v' => this.copy(y = this.y - 1)
      case '<' => this.copy(x = this.x - 1)
      case '>' => this.copy(x = this.x + 1)
      case _ => this
    }
  }

  def run(filename: String): (Int, Int) = {
    def buildPath(directions: String, currentPath: Seq[Pos]): Seq[Pos] = {
      if (directions.size == 0)
        currentPath
      else {
        val nextPos = currentPath.last.next(directions.head)
        buildPath(directions.tail, currentPath :+ nextPos)
      }
    }

    val directions = io.Source.fromFile(filename).getLines.toList.mkString

    val initialPos = Pos(0, 0)
    val path = buildPath(directions, Seq(initialPos))
    val visited = path.toSet.size

    val evenDirections = directions.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    val oddDirections = directions.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString

    val evenPath = buildPath(evenDirections, Seq(initialPos))
    val oddPath = buildPath(oddDirections, Seq(initialPos))
    val visited2 = (evenPath.toSet ++ oddPath.toSet).size

    (visited, visited2)
  }
}
