package net.jcazevedo.adventofcode

class Day12 extends DailyChallenge[Int, Int] {
  type Register = Char

  sealed trait Instruction
  sealed trait Copy[T] extends Instruction {
    def from: T
    def to: Register
  }
  sealed trait ConditionalJump[T] extends Instruction {
    def cond: T
    def offset: Int
  }
  case class CopyInteger(from: Int, to: Register) extends Copy[Int]
  case class CopyRegister(from: Register, to: Register) extends Copy[Register]
  case class Increment(reg: Register) extends Instruction
  case class Decrement(reg: Register) extends Instruction
  case class ConditionalJumpInteger(cond: Int, offset: Int) extends ConditionalJump[Int]
  case class ConditionalJumpRegister(cond: Register, offset: Int) extends ConditionalJump[Register]

  case class Program(
      registers: Map[Register, Int],
      pc: Int,
      instructions: IndexedSeq[Instruction]) {
    lazy val halts: Boolean = pc >= instructions.length

    def applyNext: Program = {
      if (halts)
        this
      else {
        instructions(pc) match {
          case CopyInteger(from, to) =>
            this.copy(registers = this.registers.updated(to, from), pc = this.pc + 1, instructions)
          case CopyRegister(from, to) =>
            this.copy(registers = this.registers.updated(to, this.registers(from)), pc = this.pc + 1, instructions)
          case Increment(reg) =>
            this.copy(registers = this.registers.updated(reg, this.registers(reg) + 1), pc = this.pc + 1, instructions)
          case Decrement(reg) =>
            this.copy(registers = this.registers.updated(reg, this.registers(reg) - 1), pc = this.pc + 1, instructions)
          case ConditionalJumpInteger(cond, offset) =>
            val o = if (cond != 0) offset else 1
            this.copy(registers, pc = this.pc + o, instructions)
          case ConditionalJumpRegister(cond, offset) =>
            val o = if (registers(cond) != 0) offset else 1
            this.copy(registers, pc = this.pc + o, instructions)
        }
      }
    }
  }

  def run(filename: String): (Int, Int) = {
    val instructionStrings = io.Source.fromFile(filename).getLines.toIndexedSeq
    val instructions = instructionStrings.map { instruction =>
      val ss = instruction.split("\\s+")
      ss(0) match {
        case "cpy" =>
          if (ss(1)(0).isLetter)
            CopyRegister(ss(1)(0), ss(2)(0))
          else
            CopyInteger(ss(1).toInt, ss(2)(0))
        case "inc" => Increment(ss(1)(0))
        case "dec" => Decrement(ss(1)(0))
        case "jnz" =>
          if (ss(1)(0).isLetter)
            ConditionalJumpRegister(ss(1)(0), ss(2).toInt)
          else
            ConditionalJumpInteger(ss(1).toInt, ss(2).toInt)
      }
    }
    var program1 = Program(Map('a' -> 0, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0, instructions)
    while (!program1.halts) {
      program1 = program1.applyNext
    }
    var program2 = Program(Map('a' -> 0, 'b' -> 0, 'c' -> 1, 'd' -> 0), 0, instructions)
    while (!program2.halts) {
      program2 = program2.applyNext
    }
    (program1.registers('a'), program2.registers('a'))
  }
}
