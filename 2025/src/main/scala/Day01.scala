object Day01 extends DailyChallenge[Int, Int] {
  enum Direction {
    case Left, Right
  }

  case class Instruction(dir: Direction, steps: Int)

  def parse(input: String): List[Instruction] =
    input
      .split("\n")
      .map(line => {
        val d = line(0)
        val rest = line.tail
        Instruction(if (d == 'L') Direction.Left else Direction.Right, rest.toInt)
      })
      .toList

  def part1(instructions: List[Instruction], dial: Int = 50): Int =
    instructions match {
      case Nil =>
        0
      case Instruction(dir, steps) :: t =>
        val next = dir match {
          case Direction.Left  => (dial + 100 - steps % 100) % 100
          case Direction.Right => (dial + steps) % 100
        }
        (if (next == 0) 1 else 0) + part1(t, next)
    }

  def part2(instructions: List[Instruction], dial: Int = 50): Int =
    instructions match {
      case Nil =>
        0
      case Instruction(Direction.Left, steps) :: t =>
        val cnt = ((100 - dial) % 100 + steps) / 100
        cnt + part2(t, (dial + 100 - steps % 100) % 100)
      case Instruction(Direction.Right, steps) :: t =>
        val cnt = (dial + steps) / 100
        cnt + part2(t, (dial + steps) % 100)
    }

  def run(input: String): (Int, Int) = {
    val instructions = parse(input)
    (part1(instructions), part2(instructions))
  }
}
