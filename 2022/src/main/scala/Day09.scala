object Day09 extends DailyChallenge[Int, Int] {
  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def touching(other: Point): Boolean = math.max(math.abs(x - other.x), math.abs(y - other.y)) < 2
  }

  case class Instruction(direction: Point, amount: Int)

  def getTailPath(
      rope: Vector[Point],
      instructions: List[Instruction],
      curr: Vector[Point] = Vector.empty
  ): Vector[Point] =
    instructions match {
      case Nil => curr :+ rope.last
      case Instruction(direction, amount) :: t =>
        val nextRope = rope.tail.foldLeft(Vector(rope.head + direction))((curr, tail) =>
          curr :+ (if (tail.touching(curr.last)) tail
                   else tail + Point(math.signum(curr.last.x - tail.x), math.signum(curr.last.y - tail.y)))
        )
        getTailPath(nextRope, if (amount > 1) Instruction(direction, amount - 1) :: t else t, curr :+ rope.last)
    }

  def run(input: String): (Int, Int) = {
    val instructions = input
      .split("\n")
      .map(line => {
        val Array(code, amount) = line.split("\\s+")
        code match {
          case "L" => Instruction(Point(-1, 0), amount.toInt)
          case "R" => Instruction(Point(1, 0), amount.toInt)
          case "U" => Instruction(Point(0, 1), amount.toInt)
          case "D" => Instruction(Point(0, -1), amount.toInt)
        }
      })
      .toList

    val part1 = getTailPath(Vector.fill(2)(Point(0, 0)), instructions).toSet.size
    val part2 = getTailPath(Vector.fill(10)(Point(0, 0)), instructions).toSet.size

    (part1, part2)
  }
}
