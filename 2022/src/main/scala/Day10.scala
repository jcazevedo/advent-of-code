object Day10 extends DailyChallenge[Long, String] {
  sealed trait Instruction {
    def cycles: Int
  }

  case object Noop extends Instruction {
    val cycles: Int = 1
  }

  case class Addx(v: Long) extends Instruction {
    val cycles: Int = 2
  }

  case class CPU(register: Long)

  object CPU {
    final val init: CPU = CPU(1)
  }

  def execute(cpu: CPU, instructions: List[Instruction]): LazyList[CPU] =
    instructions match {
      case Nil                   => LazyList(cpu)
      case Noop :: next          => LazyList.fill(Noop.cycles)(cpu) ++ execute(cpu, next)
      case (i @ Addx(v)) :: next => LazyList.fill(i.cycles)(cpu) ++ execute(cpu.copy(register = cpu.register + v), next)
    }

  def draw(instructions: List[Instruction]): String =
    execute(CPU.init, instructions).zipWithIndex
      .foldLeft("." * 240)({
        case (crt, (CPU(v), cycle)) => {
          val currCycle = cycle % 40
          if (currCycle >= v - 1 && currCycle <= v + 1) crt.updated(currCycle + (cycle / 40 * 40), '#')
          else crt
        }
      })
      .sliding(40, 40)
      .mkString("\n", "\n", "")

  def run(input: String): (Long, String) = {
    val noopR = "^noop$".r
    val addxR = "^addx (-?\\d+)$".r

    val instructions = input
      .split("\n")
      .map(_.trim)
      .map({ line =>
        line match {
          case addxR(v) => Addx(v.toLong)
          case noopR    => Noop
        }
      })
      .toList

    val execution = execute(CPU.init, instructions)
    val part1 = (20 to 220 by 40).map(cycle => execution(cycle - 1).register * cycle).sum
    val part2 = draw(instructions)

    (part1, part2)
  }
}
