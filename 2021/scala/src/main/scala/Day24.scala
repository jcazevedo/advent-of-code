import scala.util.Try
import scala.collection.mutable

object Day24 extends DailyChallenge[String, String] {
  sealed trait Instruction
  sealed trait Terminal
  case class Var(name: String) extends Terminal
  case class Num(value: Long) extends Terminal
  case class Inp(variable: Var) extends Instruction
  case class Add(variable: Var, terminal: Terminal) extends Instruction
  case class Mul(variable: Var, terminal: Terminal) extends Instruction
  case class Div(variable: Var, terminal: Terminal) extends Instruction
  case class Mod(variable: Var, terminal: Terminal) extends Instruction
  case class Eql(variable: Var, terminal: Terminal) extends Instruction

  case class ALU(variables: Map[String, Long])

  object ALU {
    final val init: ALU = ALU(Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L))
  }

  def solve(initialInstructions: List[Instruction], inputRange: LazyList[Int]): String = {
    val visited = mutable.Map.empty[Int, Set[ALU]]

    def aux(alu: ALU, instructions: List[Instruction], currentInput: String): Option[String] = {
      def valueOf(term: Terminal): Long =
        term match {
          case Var(name)  => alu.variables(name)
          case Num(value) => value
        }

      instructions match {
        case Inp(Var(a)) :: next =>
          if (currentInput.nonEmpty && visited.get(currentInput.length).exists(_.contains(alu)))
            None
          else {
            if (currentInput.nonEmpty)
              visited(currentInput.length) = visited.getOrElse(currentInput.length, Set.empty) + alu

            inputRange
              .map(nextInput => aux(ALU(alu.variables.updated(a, nextInput)), next, s"$currentInput$nextInput"))
              .find(_.nonEmpty)
              .flatten
          }

        case Add(Var(a), term) :: next =>
          aux(ALU(alu.variables.updated(a, alu.variables(a) + valueOf(term))), next, currentInput)

        case Mul(Var(a), term) :: next =>
          aux(ALU(alu.variables.updated(a, alu.variables(a) * valueOf(term))), next, currentInput)

        case Div(Var(a), term) :: next =>
          aux(ALU(alu.variables.updated(a, alu.variables(a) / valueOf(term))), next, currentInput)

        case Mod(Var(a), term) :: next =>
          aux(ALU(alu.variables.updated(a, alu.variables(a) % valueOf(term))), next, currentInput)

        case Eql(Var(a), term) :: next =>
          aux(ALU(alu.variables.updated(a, if (alu.variables(a) == valueOf(term)) 1L else 0L)), next, currentInput)

        case Nil if alu.variables("z") == 0L =>
          Some(currentInput)

        case Nil =>
          None
      }
    }

    aux(ALU.init, initialInstructions, "").get
  }

  def run(input: String): (String, String) = {
    val instructions: List[Instruction] = input
      .split("\n")
      .map({
        case s"inp $a"    => Inp(Var(a))
        case s"add $a $b" => Add(Var(a), Try(b.toLong).map(Num(_)).getOrElse(Var(b)))
        case s"mul $a $b" => Mul(Var(a), Try(b.toLong).map(Num(_)).getOrElse(Var(b)))
        case s"div $a $b" => Div(Var(a), Try(b.toLong).map(Num(_)).getOrElse(Var(b)))
        case s"mod $a $b" => Mod(Var(a), Try(b.toLong).map(Num(_)).getOrElse(Var(b)))
        case s"eql $a $b" => Eql(Var(a), Try(b.toLong).map(Num(_)).getOrElse(Var(b)))
      })
      .toList

    val part1 = solve(instructions, (9 to 1 by -1).to(LazyList))
    val part2 = solve(instructions, (1 to 9 by 1).to(LazyList))

    (part1, part2)
  }
}
