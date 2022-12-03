object Day05 extends DailyChallenge[Int, Int] {
  case class Point(x: Int, y: Int) {
    def stepTo(to: Point): Point = Point(x + math.signum(to.x - x), y + math.signum(to.y - y))
  }

  case class Line(from: Point, to: Point) {
    def isHorizontal: Boolean = from.y == to.y
    def isVertical: Boolean = from.x == to.x
    def points: List[Point] = {
      def aux(curr: Point): List[Point] =
        if (curr == to) List(curr)
        else curr :: aux(curr.stepTo(to))

      aux(from)
    }
  }

  def overlappingPoints(lines: List[Line]): Int = {
    val overlaps = lines.foldLeft(Map.empty[Point, Int])((curr1, line) =>
      line.points.foldLeft(curr1)((curr2, point) => curr2.updated(point, curr2.getOrElse(point, 0) + 1))
    )
    overlaps.filter(_._2 > 1).size
  }

  def run(input: String): (Int, Int) = {
    val lines = input
      .split("\n")
      .map(line => {
        val Array(p1, p2) = line.split(" -> ")
        val Array(x1, y1) = p1.split(",")
        val Array(x2, y2) = p2.split(",")
        Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      })
      .toList

    val part1 = overlappingPoints(lines.filter(line => line.isHorizontal || line.isVertical))
    val part2 = overlappingPoints(lines)

    (part1, part2)
  }
}
