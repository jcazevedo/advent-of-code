package adventofcode

class Day12 extends DailyChallenge[Int, Int] {
  sealed trait Instruction
  case class N(v: Int) extends Instruction
  case class S(v: Int) extends Instruction
  case class E(v: Int) extends Instruction
  case class W(v: Int) extends Instruction
  case class L(v: Int) extends Instruction
  case class R(v: Int) extends Instruction
  case class F(v: Int) extends Instruction

  sealed trait Dir {
    def left(n: Int) = Dir.dirs((Dir.dirs.indexOf(this) + 4 - n % 4) % 4)
    def right(n: Int) = Dir.dirs((Dir.dirs.indexOf(this) + n % 4) % 4)
  }
  case object East extends Dir
  case object South extends Dir
  case object West extends Dir
  case object North extends Dir

  object Dir {
    val dirs = List(East, South, West, North)
  }

  case class ShipState(x: Int, y: Int, dir: Dir)

  def part1(instructions: List[Instruction]): Int = {
    val initialSt = ShipState(0, 0, East)
    val finalSt = instructions.foldLeft(initialSt) {
      case (state, N(v))                  => state.copy(y = state.y + v)
      case (state, S(v))                  => state.copy(y = state.y - v)
      case (state, E(v))                  => state.copy(x = state.x + v)
      case (state, W(v))                  => state.copy(x = state.x - v)
      case (state, R(v))                  => state.copy(dir = state.dir.right(v / 90))
      case (state, L(v))                  => state.copy(dir = state.dir.left(v / 90))
      case (ShipState(x, y, East), F(v))  => ShipState(x + v, y, East)
      case (ShipState(x, y, South), F(v)) => ShipState(x, y - v, South)
      case (ShipState(x, y, West), F(v))  => ShipState(x - v, y, West)
      case (ShipState(x, y, North), F(v)) => ShipState(x, y + v, North)
    }
    math.abs(finalSt.x) + math.abs(finalSt.y)
  }

  case class ShipStateWithWaypoint(sx: Int, sy: Int, wx: Int, wy: Int)

  def part2(instructions: List[Instruction]): Int = {
    val initialSt = (ShipStateWithWaypoint(0, 0, 10, 1))
    val finalSt = instructions.foldLeft(initialSt) { case (state, op) =>
      op match {
        case N(v) => state.copy(wy = state.wy + v)
        case S(v) => state.copy(wy = state.wy - v)
        case E(v) => state.copy(wx = state.wx + v)
        case W(v) => state.copy(wx = state.wx - v)
        case L(v) => (0 until v / 90).foldLeft(state) { case (s, _) => s.copy(wx = -s.wy, wy = s.wx) }
        case R(v) => (0 until v / 90).foldLeft(state) { case (s, _) => s.copy(wx = s.wy, wy = -s.wx) }
        case F(v) => state.copy(sx = state.sx + state.wx * v, sy = state.sy + state.wy * v)
      }
    }
    math.abs(finalSt.sx) + math.abs(finalSt.sy)
  }

  def run(input: String): (Int, Int) = {
    val instructions = input
      .split("\n")
      .map { str =>
        val v = str.drop(1).toInt
        str(0) match {
          case 'N' => N(v)
          case 'S' => S(v)
          case 'E' => E(v)
          case 'W' => W(v)
          case 'L' => L(v)
          case 'R' => R(v)
          case 'F' => F(v)
        }
      }
      .toList
    (part1(instructions), part2(instructions))
  }
}
