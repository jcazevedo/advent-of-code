import scala.collection.mutable

object Day18 extends DailyChallenge[Int, Int] {
  case class Point(x: Int, y: Int, z: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
  }

  val Directions = List(
    Point(1, 0, 0),
    Point(-1, 0, 0),
    Point(0, 1, 0),
    Point(0, -1, 0),
    Point(0, 0, 1),
    Point(0, 0, -1)
  )

  def inside(point: Point, points: Set[Point]): Boolean = {
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max
    val minZ = points.map(_.z).min
    val maxZ = points.map(_.z).max

    def outsideBoundary(p: Point): Boolean =
      p.x < minX || p.x > maxX || p.y < minY || p.y > maxY || p.z < minZ || p.z > maxZ

    val visited = mutable.Set.empty[Point]

    def flood(p: Point): Boolean = {
      if (points.contains(p) || visited(p)) true
      else if (outsideBoundary(p)) false
      else {
        visited.add(p)
        Directions.forall(dir => flood(p + dir))
      }
    }

    flood(point)
  }

  def run(input: String): (Int, Int) = {
    val points = input
      .split("\n")
      .map(_.trim)
      .map { line =>
        val Array(x, y, z) = line.split(",")
        Point(x.toInt, y.toInt, z.toInt)
      }
      .toSet

    val part1 = points.size * 6 - points.toList.map(p1 => Directions.count(dir => points.contains(p1 + dir))).sum
    val part2 = part1 - points
      .flatMap(p => Directions.map(p + _).toSet)
      .filter(p => !points.contains(p))
      .filter(p => inside(p, points))
      .toList
      .map(p1 => Directions.count(dir => points.contains(p1 + dir)))
      .sum

    (part1, part2)
  }
}
