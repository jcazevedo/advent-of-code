object Day14 extends DailyChallenge[Int, Int] {
  case class Point(x: Int, y: Int) {
    def to(other: Point): List[Point] =
      if (x == other.x) (math.min(y, other.y) to math.max(y, other.y)).map(ny => Point(x, ny)).toList
      else (math.min(x, other.x) to math.max(x, other.x)).map(nx => Point(nx, y)).toList

    def down: Point = Point(x, y + 1)
    def left: Point = Point(x - 1, y)
    def right: Point = Point(x + 1, y)
  }

  sealed trait Block
  case object Rock extends Block
  case object Sand extends Block

  sealed trait Cave {
    def blockedAt(p: Point): Boolean
    def sandBlocks: Int
    def maxHeight: Int
    def add(p: Point, b: Block): Cave
  }

  case class UnlimitedCave(blocks: Map[Point, Block], maxHeight: Int) extends Cave {
    def blockedAt(p: Point): Boolean = blocks.contains(p)
    lazy val sandBlocks: Int = blocks.count(_._2 == Sand)
    def add(p: Point, b: Block): UnlimitedCave =
      UnlimitedCave(blocks.updated(p, b), if (b == Rock) math.max(maxHeight, p.y) else maxHeight)
    def limited: Cave = LimitedCave(blocks, maxHeight + 2)
  }

  case class LimitedCave(blocks: Map[Point, Block], maxHeight: Int) extends Cave {
    def blockedAt(p: Point): Boolean = blocks.contains(p) || p.y == maxHeight
    lazy val sandBlocks: Int = blocks.count(_._2 == Sand)
    def add(p: Point, b: Block): LimitedCave =
      LimitedCave(blocks.updated(p, b), if (b == Rock) math.max(maxHeight, p.y + 2) else maxHeight)
  }

  def dropSand(cave: Cave): Cave = {
    def dropAt(currSand: Point): Option[Point] =
      if (currSand.y > cave.maxHeight) None
      else if (!cave.blockedAt(currSand.down)) dropAt(currSand.down)
      else if (!cave.blockedAt(currSand.down.left)) dropAt(currSand.down.left)
      else if (!cave.blockedAt(currSand.down.right)) dropAt(currSand.down.right)
      else if (!cave.blockedAt(currSand)) Some(currSand)
      else None

    dropAt(Point(500, 0)).fold(cave)(cave.add(_, Sand))
  }

  def dropAll(cave: Cave): Cave = {
    val nextCave = dropSand(cave)
    if (nextCave.sandBlocks == cave.sandBlocks) cave
    else dropAll(nextCave)
  }

  def run(input: String): (Int, Int) = {
    val cave = input
      .split("\n")
      .foldLeft(UnlimitedCave(Map.empty, 0))((cave, line) =>
        line
          .split(" -> ")
          .map(coords => {
            val Array(x, y) = coords.split(",").map(_.toInt)
            Point(x, y)
          })
          .sliding(2)
          .foldLeft(cave)({ case (cave, Array(from, to)) => from.to(to).foldLeft(cave)(_.add(_, Rock)) })
      )

    val part1 = dropAll(cave).sandBlocks
    val part2 = dropAll(cave.limited).sandBlocks

    (part1, part2)
  }
}
