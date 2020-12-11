package adventofcode

class Day11 extends DailyChallenge[Int, Int] {
  val dirs = List((-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1))

  def charAt(grid: Vector[String], i: Int, j: Int): Char = {
    if (i < 0 || i >= grid.length || j < 0 || j >= grid(i).length) '.'
    else grid(i)(j)
  }

  def seen(grid: Vector[String], i: Int, j: Int, di: Int, dj: Int): Char = {
    val ni = i + di
    val nj = j + dj
    if (ni < 0 || ni >= grid.length || nj < 0 || nj >= grid(ni).length) '.'
    else if (grid(ni)(nj) != '.') grid(ni)(nj)
    else seen(grid, ni, nj, di, dj)
  }

  def run(grid: Vector[String], simulateF: Vector[String] => Vector[String]): Vector[String] = {
    val next = simulateF(grid)
    if (next != grid) run(next, simulateF)
    else next
  }

  def part1(grid: Vector[String]): Int = {
    def simulate(grid: Vector[String]): Vector[String] = {
      grid.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (ch, j) =>
          val adjacent = dirs.map { case (di, dj) => charAt(grid, i + di, j + dj) }
          ch match {
            case 'L' if adjacent.count(_ == '#') == 0 => '#'
            case '#' if adjacent.count(_ == '#') >= 4 => 'L'
            case _                                    => ch
          }
        }.mkString
      }
    }

    run(grid, simulate).map(_.count(_ == '#')).sum
  }

  def part2(grid: Vector[String]): Int = {
    def simulate(grid: Vector[String]): Vector[String] = {
      grid.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (ch, j) =>
          val adjacent = dirs.map { case (di, dj) => seen(grid, i, j, di, dj) }
          ch match {
            case 'L' if adjacent.count(_ == '#') == 0 => '#'
            case '#' if adjacent.count(_ == '#') >= 5 => 'L'
            case _                                    => ch
          }
        }.mkString
      }
    }

    run(grid, simulate).map(_.count(_ == '#')).sum
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").toVector
    (part1(grid), part2(grid))
  }
}
