import scala.collection.mutable

object Day08 extends DailyChallenge[Int, Int] {
  def buildMap(grid: Vector[(Vector[(Char, Int)], Int)]): Map[Char, Vector[(Int, Int)]] = {
    val m = mutable.Map.empty[Char, Vector[(Int, Int)]]
    grid.foreach { (line, i) =>
      line.foreach { (ch, j) =>
        if (ch != '.') m.updateWith(ch) {
          case Some(vec) => Some(vec :+ (i, j))
          case None      => Some(Vector((i, j)))
        }
      }
    }
    m.toMap
  }

  def run(input: String): (Int, Int) = {
    val Grid = input.split("\n").map(_.toVector).toVector
    val Rows = Grid.length
    val Cols = Grid(0).length

    def inside(p: (Int, Int)): Boolean =
      p._1 >= 0 && p._1 < Rows && p._2 >= 0 && p._2 < Cols

    val antennas = buildMap(Grid.map(_.zipWithIndex).zipWithIndex)

    val antinodeLocations = antennas.foldLeft(Set.empty[(Int, Int)]) { case (acc, (_, vec)) =>
      (for {
        antenna1 @ (a1i, a1j) <- vec
        antenna2 @ (a2i, a2j) <- vec
        if antenna1 != antenna2
        p1 = (2 * a1i - a2i, 2 * a1j - a2j)
        p2 = (2 * a2i - a1i, 2 * a2j - a1j)
      } yield Set(p1, p2)).flatten.toSet.filter(inside) ++ acc
    }

    val resonantAntinodeLocations = antennas.foldLeft(Set.empty[(Int, Int)]) { case (acc, (_, vec)) =>
      (for {
        antenna1 @ (a1i, a1j) <- vec
        antenna2 @ (a2i, a2j) <- vec
        if antenna1 != antenna2
        (d1i, d1j) = (a1i - a2i, a1j - a2j)
        antinodes1 = Iterator.iterate(antenna1)((i, j) => (i + d1i, j + d1j)).takeWhile(inside).toSet
        (d2i, d2j) = (a2i - a1i, a2j - a1j)
        antinodes2 = Iterator.iterate(antenna2)((i, j) => (i + d2i, j + d2j)).takeWhile(inside).toSet
      } yield antinodes1 ++ antinodes2).flatten.toSet ++ acc
    }

    val part1 = antinodeLocations.size
    val part2 = resonantAntinodeLocations.size

    (part1, part2)
  }
}
