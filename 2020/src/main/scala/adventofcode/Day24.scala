package adventofcode

class Day24 extends DailyChallenge[Int, Int] {
  sealed trait Dir
  case object E extends Dir
  case object SE extends Dir
  case object SW extends Dir
  case object W extends Dir
  case object NW extends Dir
  case object NE extends Dir
  object Dir {
    val all = List(E, SE, SW, W, NW, NE)
  }

  case class Coord(x: Int, y: Int) {
    def next(dir: Dir): Coord = {
      dir match {
        case E  => Coord(x + 1, y)
        case SE => if (y % 2 == 0) Coord(x, y + 1) else Coord(x + 1, y + 1)
        case SW => if (y % 2 == 0) Coord(x - 1, y + 1) else Coord(x, y + 1)
        case W  => Coord(x - 1, y)
        case NW => if (y % 2 == 0) Coord(x - 1, y - 1) else Coord(x, y - 1)
        case NE => if (y % 2 == 0) Coord(x, y - 1) else Coord(x + 1, y - 1)
      }
    }
  }

  def goto(coord: Coord, path: List[Dir]): Coord =
    path.foldLeft(coord)(_.next(_))

  def getInitialState(paths: List[List[Dir]]): Set[Coord] =
    paths
      .foldLeft(Set.empty[Coord]) { case (black, path) =>
        val target = goto(Coord(0, 0), path)
        if (black(target)) black - target else black + target
      }

  def part1(paths: List[List[Dir]]): Int =
    getInitialState(paths).size

  def part2(paths: List[List[Dir]]): Int = {
    val init = getInitialState(paths)

    def simulate(black: Set[Coord], times: Int): Set[Coord] =
      if (times == 0) black
      else {
        val remainingBlack = black.filter { tile =>
          val bc = Dir.all.count(d => black(tile.next(d)))
          bc >= 1 && bc <= 2
        }
        val newBlack = for {
          tile <- black
          whiteTile <- Dir.all.map(tile.next).filter(t => !black(t))
          if Dir.all.map(whiteTile.next).count(black) == 2
        } yield whiteTile
        simulate(remainingBlack ++ newBlack, times - 1)
      }

    simulate(init, 100).size
  }

  def run(input: String): (Int, Int) = {
    val paths = input
      .split("\n")
      .map {
        _.foldLeft((List.empty[Dir], Option.empty[Char])) { case ((dirs, acc), ch) =>
          ((acc, ch): @unchecked) match {
            case (Some('s'), 'e') => (dirs :+ SE, None)
            case (Some('s'), 'w') => (dirs :+ SW, None)
            case (Some('n'), 'e') => (dirs :+ NE, None)
            case (Some('n'), 'w') => (dirs :+ NW, None)
            case (None, 'e')      => (dirs :+ E, None)
            case (None, 'w')      => (dirs :+ W, None)
            case (_, 's')         => (dirs, Some('s'))
            case (_, 'n')         => (dirs, Some('n'))
          }
        }._1
      }
      .toList

    (part1(paths), part2(paths))
  }
}
