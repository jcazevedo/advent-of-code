object Day23 extends DailyChallenge[Int, Int] {
  case class Point(i: Int, j: Int) {
    def +(vec: Vec): Point = Point(i + vec.i, j + vec.j)
  }

  case class Vec(i: Int, j: Int)

  case class Check(directions: List[Vec], move: Vec)

  final val N: Vec = Vec(-1, 0)
  final val NE: Vec = Vec(-1, 1)
  final val E: Vec = Vec(0, 1)
  final val SE: Vec = Vec(1, 1)
  final val S: Vec = Vec(1, 0)
  final val SW: Vec = Vec(1, -1)
  final val W: Vec = Vec(0, -1)
  final val NW: Vec = Vec(-1, -1)

  final val Directions: List[Vec] = List(N, NE, E, SE, S, SW, W, NW)
  final val Checks: List[Check] =
    List(Check(List(N, NE, NW), N), Check(List(S, SE, SW), S), Check(List(W, NW, SW), W), Check(List(E, NE, SE), E))

  def moveOne(curr: Set[Point], checks: LazyList[Check]): Set[Point] = {
    val currentChecks = checks.take(4).toList
    val proposals = curr.foldLeft(Map.empty[Point, Point])((acc, elf) =>
      if (Directions.map(elf + _).forall(p => !curr.contains(p))) acc.updated(elf, elf)
      else {
        val proposalOpt = currentChecks.find({ case Check(directions, move) =>
          directions.map(elf + _).forall(p => !curr.contains(p))
        })
        acc.updated(elf, proposalOpt.fold(elf)(elf + _.move))
      }
    )
    val groupedProposals = proposals.groupBy(_._2)
    curr.map(elf => if (groupedProposals(proposals(elf)).size > 1) elf else proposals(elf))
  }

  def rounds(start: Set[Point]): LazyList[Set[Point]] = {
    def checks: LazyList[Check] = Checks.to(LazyList) #::: checks
    LazyList.iterate((start, checks))({ case (curr, checks) => (moveOne(curr, checks), checks.tail) }).map(_._1)
  }

  def emptyGround(elves: Set[Point]): Int = {
    val ii = elves.map(_.i)
    val jj = elves.map(_.j)

    val minI = ii.min
    val maxI = ii.max
    val minJ = jj.min
    val maxJ = jj.max

    (maxJ - minJ + 1) * (maxI - minI + 1) - elves.size
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").map(_.trim).toVector
    val elves = grid.zipWithIndex.flatMap { case (line, i) =>
      line.zipWithIndex.flatMap {
        case ('#', j) => Some(Point(i, j))
        case ('.', j) => None
      }
    }.toSet

    val part1 =
      emptyGround(rounds(elves).take(11).last)
    val part2 =
      rounds(elves).sliding(2).map(_.toList).to(LazyList).takeWhile(l => l.head != l.tail.head).zipWithIndex.last._2 + 2

    (part1, part2)
  }
}
