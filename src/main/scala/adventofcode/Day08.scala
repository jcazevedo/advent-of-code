package adventofcode

class Day08 extends DailyChallenge[Int, Int] {
  sealed trait Instruction
  case class Acc(arg: Int) extends Instruction
  case class Jmp(arg: Int) extends Instruction
  case class Nop(arg: Int) extends Instruction

  case class Result(inLoop: Boolean, accumulator: Int)

  def run(instructions: Vector[Instruction]): Result = {
    def run(pc: Int, acc: Int = 0, visited: Set[Int] = Set.empty): Result = {
      if (pc >= instructions.length) Result(inLoop = false, acc)
      else if (visited(pc)) Result(inLoop = true, acc)
      else
        instructions(pc) match {
          case Acc(arg) => run(pc + 1, acc + arg, visited + pc)
          case Jmp(arg) => run(pc + arg, acc, visited + pc)
          case Nop(_)   => run(pc + 1, acc, visited + pc)
        }
    }
    run(0)
  }

  def part1(instructions: Vector[Instruction]): Int =
    run(instructions).accumulator

  def part2(instructions: Vector[Instruction]): Int =
    instructions.zipWithIndex.collectFirst {
      case (Jmp(arg), idx) if !run(instructions.updated(idx, Nop(arg))).inLoop =>
        run(instructions.updated(idx, Nop(arg))).accumulator
      case (Nop(arg), idx) if !run(instructions.updated(idx, Jmp(arg))).inLoop =>
        run(instructions.updated(idx, Jmp(arg))).accumulator
    }.get

  def run(input: String): (Int, Int) = {
    val instructions = input
      .split("\n")
      .map { str =>
        val Array(op, arg) = str.split(" ")
        (op: @unchecked) match {
          case "acc" => Acc(arg.toInt)
          case "jmp" => Jmp(arg.toInt)
          case "nop" => Nop(arg.toInt)
        }
      }
      .toVector
    (part1(instructions), part2(instructions))
  }
}
