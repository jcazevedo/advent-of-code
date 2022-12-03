object Day02 extends DailyChallenge[Long, Long] {
  sealed trait Instruction
  case class Forward(units: Long) extends Instruction
  case class Down(units: Long) extends Instruction
  case class Up(units: Long) extends Instruction

  case class State(horizontalPosition: Long, depth: Long, aim: Long) {
    def apply(instruction: Instruction): State =
      instruction match {
        case Forward(units) => copy(horizontalPosition = this.horizontalPosition + units)
        case Down(units)    => copy(depth = this.depth + units)
        case Up(units)      => copy(depth = this.depth - units)
      }

    def applyWithAim(instruction: Instruction): State =
      instruction match {
        case Forward(units) =>
          copy(horizontalPosition = this.horizontalPosition + units, depth = this.depth + aim * units)
        case Down(units) => copy(aim = this.aim + units)
        case Up(units)   => copy(aim = this.aim - units)
      }
  }

  object State {
    final val initial: State = State(0L, 0L, 0L)
  }

  def run(input: String): (Long, Long) = {
    val instructions = input
      .split("\n")
      .toList
      .map(line => {
        val Array(cmd, unit) = line.split(" ")
        cmd match {
          case "forward" => Forward(unit.toLong)
          case "down"    => Down(unit.toLong)
          case "up"      => Up(unit.toLong)
        }
      })

    val finalState = instructions.foldLeft(State.initial)(_.apply(_))
    val part1 = finalState.horizontalPosition * finalState.depth

    val finalStateWithAim = instructions.foldLeft(State.initial)(_.applyWithAim(_))
    val part2 = finalStateWithAim.horizontalPosition * finalStateWithAim.depth

    (part1, part2)
  }
}
