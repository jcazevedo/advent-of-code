package adventofcode

class Day08 extends DailyChallenge[Int, Int] {
  sealed trait Instruction
  case class Acc(arg: Int) extends Instruction
  case class Jmp(arg: Int) extends Instruction
  case class Nop(arg: Int) extends Instruction

  case class Result(inLoop: Boolean, accumulator: Int)

  def run(instructions: Vector[Instruction]): Result = {
    def aux(pc: Int, acc: Int = 0, visited: Set[Int] = Set.empty): Result = {
      if (pc >= instructions.length) Result(inLoop = false, acc)
      else if (visited(pc)) Result(inLoop = true, acc)
      else {
        assert(pc >= 0 && pc < instructions.length)
        instructions(pc) match {
          case Acc(arg) => aux(pc + 1, acc + arg, visited + pc)
          case Jmp(arg) => aux(pc + arg, acc, visited + pc)
          case Nop(_)   => aux(pc + 1, acc, visited + pc)
        }
      }
    }

    aux(0)
  }

  def part1(instructions: Vector[Instruction]): Int =
    run(instructions).accumulator

  def part2(instructions: Vector[Instruction]): Int = {
    val ans = instructions.zipWithIndex.foldLeft[Option[Int]](None) {
      case (Some(v), _) =>
        Some(v)
      case (None, (Jmp(arg), idx)) =>
        val res = run(instructions.updated(idx, Nop(arg)))
        Some(res.accumulator).filter(_ => !res.inLoop)
      case (None, (Nop(arg), idx)) =>
        val res = run(instructions.updated(idx, Jmp(arg)))
        Some(res.accumulator).filter(_ => !res.inLoop)
      case (None, _) =>
        None
    }

    assert(ans.nonEmpty)
    ans.get
  }

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
