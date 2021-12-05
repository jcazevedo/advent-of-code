package adventofcode

class Day20 extends DailyChallenge[Long, Int] {
  case class Tile(id: Long, grid: Vector[String])

  def rotateR(grid: Vector[String]): Vector[String] =
    (for {
      j <- grid.indices
      line = grid.map(_(j)).reverse.mkString
    } yield line).toVector

  def flip(grid: Vector[String]): Vector[String] =
    grid.reverse

  def variations(grid: Vector[String]): List[Vector[String]] = {
    val rotations = Iterator.unfold(grid)(grid => Some((grid, rotateR(grid)))).take(4).toList
    val flips = rotations.map(flip)
    rotations ++ flips
  }

  def mkTile(str: String): Tile = {
    val strs = str.split("\n")
    val id = strs(0).stripPrefix("Tile ").stripSuffix(":").toLong
    val grid = strs.tail.toVector
    Tile(id, grid)
  }

  def getTopLine(tile: Tile): String =
    tile.grid.head

  def getLeftLine(tile: Tile): String =
    tile.grid.map(_.head).mkString

  def getRightLine(tile: Tile): String =
    tile.grid.map(_.last).mkString

  def getBottomLine(tile: Tile): String =
    tile.grid.last

  def getArrangement(tiles: List[Tile]): Vector[Vector[Tile]] = {
    val L = math.sqrt(tiles.length.toDouble).toInt
    val allTiles = tiles.flatMap(tile => variations(tile.grid).map(g => tile.copy(grid = g)))

    def go(
        i: Int,
        j: Int,
        curr: Vector[Vector[Tile]] = Vector.empty,
        used: Set[Long] = Set.empty
    ): Option[Vector[Vector[Tile]]] = {
      if (i >= L) Some(curr)
      else {
        val topTile = if (i > 0) Some(curr(i - 1)(j)) else None
        val leftTile = if (j > 0) Some(curr(i)(j - 1)) else None
        val candidates = allTiles.iterator
          .filter(t => !used(t.id))
          .filter(t => topTile.fold(true)(tile => getBottomLine(tile) == getTopLine(t)))
          .filter(t => leftTile.fold(true)(tile => getRightLine(tile) == getLeftLine(t)))
        candidates
          .map { candidate =>
            val nextV =
              if (curr.length == i) curr :+ Vector(candidate) else curr.updated(curr.length - 1, curr.last :+ candidate)
            val nj = if (j + 1 < L) j + 1 else 0
            val ni = if (j + 1 < L) i else i + 1
            go(ni, nj, nextV, used + candidate.id)
          }
          .find(_.nonEmpty)
          .flatten
      }
    }

    val targetOpt = go(0, 0)
    assert(targetOpt.nonEmpty)
    targetOpt.get
  }

  def part1(tiles: List[Tile]): Long = {
    val target = getArrangement(tiles)
    target.head.head.id * target.head.last.id * target.last.head.id * target.last.last.id
  }

  def part2(tiles: List[Tile]): Int = {
    val seaMonster = Vector(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    )
    val seaMonsterSqs = seaMonster.map(_.count(_ == '#')).sum

    val target = getArrangement(tiles)
    val noBorders = target.map(_.map(tile => tile.grid.tail.init.map(_.tail.init)))
    val joined = noBorders.flatMap(line => line(0).indices.map(i => line.map(_(i)).mkString))

    def notSeaMonsters(grid: Vector[String]): Int = {
      def removeSeaMonsters(grid: Vector[String]): Vector[String] = {
        def go(i: Int, j: Int, curr: Vector[String]): Vector[String] = {
          if (j + seaMonster(0).length >= curr(i).length)
            go(i + 1, 0, curr)
          else if (i + seaMonster.length >= curr.length)
            curr
          else {
            val hasSeaMonster = (for {
              si <- seaMonster.indices
              sj <- seaMonster(si).indices
              if seaMonster(si)(sj) == '#'
            } yield curr(i + si)(j + sj) == '#').count(identity) == seaMonsterSqs

            val nextGrid =
              if (!hasSeaMonster) curr
              else {
                seaMonster.indices.foldLeft(curr) { case (curr, si) =>
                  seaMonster(si).indices.foldLeft(curr) { case (curr, sj) =>
                    if (seaMonster(si)(sj) == '#')
                      curr.updated(i + si, curr(i + si).updated(j + sj, 'O'))
                    else
                      curr
                  }
                }
              }

            go(i, j + 1, nextGrid)
          }
        }

        go(0, 0, grid)
      }

      removeSeaMonsters(grid).map(_.count(_ == '#')).sum
    }

    variations(joined).map(notSeaMonsters).min
  }

  def run(input: String): (Long, Int) = {
    val tiles = input.trim.split("\n\n").map(mkTile).toList
    (part1(tiles), part2(tiles))
  }
}
