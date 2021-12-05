package adventofcode

class Day17 extends DailyChallenge[Int, Int] {
  case class Coord3(x: Int, y: Int, z: Int)

  def simulate3(coords: Set[Coord3], n: Int): Set[Coord3] = {
    if (n == 0) coords
    else {
      val xs = coords.map(_.x)
      val ys = coords.map(_.y)
      val zs = coords.map(_.z)
      val xMin = xs.min - 1
      val xMax = xs.max + 1
      val yMin = ys.min - 1
      val yMax = ys.max + 1
      val zMin = zs.min - 1
      val zMax = zs.max + 1
      val next = (for {
        x <- xMin to xMax
        y <- yMin to yMax
        z <- zMin to zMax
        coord = Coord3(x, y, z)
        active = coords(Coord3(x, y, z))
        activeN = (for {
          nx <- x - 1 to x + 1
          ny <- y - 1 to y + 1
          nz <- z - 1 to z + 1
          if nx != x || ny != y || nz != z
          coord = Coord3(nx, ny, nz)
          if coords(coord)
        } yield coord).toSet
        if (active && (activeN.size == 2 || activeN.size == 3)) || (!active && activeN.size == 3)
      } yield coord).toSet
      simulate3(next, n - 1)
    }
  }

  case class Coord4(x: Int, y: Int, z: Int, w: Int)

  def simulate4(coords: Set[Coord4], n: Int): Set[Coord4] = {
    if (n == 0) coords
    else {
      val xs = coords.map(_.x)
      val ys = coords.map(_.y)
      val zs = coords.map(_.z)
      val ws = coords.map(_.w)
      val xMin = xs.min - 1
      val xMax = xs.max + 1
      val yMin = ys.min - 1
      val yMax = ys.max + 1
      val zMin = zs.min - 1
      val zMax = zs.max + 1
      val wMin = ws.min - 1
      val wMax = ws.max + 1
      val next = (for {
        x <- xMin to xMax
        y <- yMin to yMax
        z <- zMin to zMax
        w <- wMin to wMax
        coord = Coord4(x, y, z, w)
        active = coords(Coord4(x, y, z, w))
        activeN = (for {
          nx <- x - 1 to x + 1
          ny <- y - 1 to y + 1
          nz <- z - 1 to z + 1
          nw <- w - 1 to w + 1
          if nx != x || ny != y || nz != z || nw != w
          coord = Coord4(nx, ny, nz, nw)
          if coords(coord)
        } yield coord).toSet
        if (active && (activeN.size == 2 || activeN.size == 3)) || (!active && activeN.size == 3)
      } yield coord).toSet
      simulate4(next, n - 1)
    }
  }

  def run(input: String): (Int, Int) = {
    val filled3 = input
      .split("\n")
      .zipWithIndex
      .flatMap { case (str, y) =>
        str.zipWithIndex.flatMap {
          case ('#', x) => Some(Coord3(x, y, 0))
          case _        => None
        }
      }
      .toSet
    val filled4 = input
      .split("\n")
      .zipWithIndex
      .flatMap { case (str, y) =>
        str.zipWithIndex.flatMap {
          case ('#', x) => Some(Coord4(x, y, 0, 0))
          case _        => None
        }
      }
      .toSet
    (simulate3(filled3, 6).size, simulate4(filled4, 6).size)
  }
}
