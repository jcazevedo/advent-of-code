package net.jcazevedo.adventofcode

class Day11 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    case class Cube(x: Int, y: Int, z: Int)

    def move(cube: Cube, dir: String): Cube =
      dir match {
        case "n" => cube.copy(y = cube.y + 1, z = cube.z - 1)
        case "ne" => cube.copy(x = cube.x + 1, z = cube.z - 1)
        case "se" => cube.copy(x = cube.x + 1, y = cube.y - 1)
        case "s" => cube.copy(y = cube.y - 1, z = cube.z + 1)
        case "sw" => cube.copy(x = cube.x - 1, z = cube.z + 1)
        case "nw" => cube.copy(x = cube.x - 1, y = cube.y + 1)
      }

    def dist(start: Cube, end: Cube): Int =
      (math.abs(start.x - end.x) + math.abs(start.y - end.y) + math.abs(start.z - end.z)) / 2

    val lines = io.Source.fromFile(filename).getLines().toList
    val directions = lines.head.split(",")
    val start = Cube(0, 0, 0)

    val (end, maxDist) = directions.foldLeft((start, 0)) {
      case ((currP, currDist), coord) =>
        val next = move(currP, coord)
        (next, math.max(currDist, dist(next, start)))
    }
    (dist(end, start), maxDist)
  }
}
