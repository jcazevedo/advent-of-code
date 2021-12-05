package net.jcazevedo.adventofcode

class Day23 extends DailyChallenge[Int, Int] {
  type Register = Char

  sealed trait Instruction
  case class Copy[T, U](from: T, to: U) extends Instruction
  case class Increment[T](v: T) extends Instruction
  case class Decrement[T](v: T) extends Instruction
  case class ConditionalJump[T, U](cond: T, offset: U) extends Instruction
  case class Toggle[T](offset: T) extends Instruction

  case class Program(
      registers: Map[Register, Int],
      pc: Int,
      instructions: IndexedSeq[Instruction]) {
    lazy val halts: Boolean = pc >= instructions.length

    def getValue[T](v: T): Int = v match {
      case x: Int => x
      case r: Register => this.registers(r)
      case o => -1
    }

    def applyNext: Program = {
      if (halts)
        this
      else {
        instructions(pc) match {
          case Copy(from, t) =>
            t match {
              case to: Register =>
                this.copy(registers = this.registers.updated(to, getValue(from)), pc = this.pc + 1, instructions)
              case _ =>
                this.copy(registers, pc = this.pc + 1, instructions)
            }
          case Increment(r) =>
            r match {
              case reg: Register =>
                this.copy(registers = this.registers.updated(reg, this.registers(reg) + 1), pc = this.pc + 1, instructions)
              case _ =>
                this.copy(registers, pc = this.pc + 1, instructions)
            }
          case Decrement(r) =>
            r match {
              case reg: Register =>
                this.copy(registers = this.registers.updated(reg, this.registers(reg) - 1), pc = this.pc + 1, instructions)
              case _ =>
                this.copy(registers, pc = this.pc + 1, instructions)
            }
          case ConditionalJump(cond, offset) =>
            val o = if (getValue(cond) != 0) getValue(offset) else 1
            this.copy(registers, pc = this.pc + o, instructions)
          case Toggle(o) =>
            val offset = getValue(o)
            if (pc + offset < 0 || pc + offset >= instructions.length)
              this.copy(registers, this.pc + 1, instructions)
            else {
              val targetInstruction = instructions(this.pc + offset)
              val nextInstruction = targetInstruction match {
                case Increment(v) => Decrement(v)
                case Decrement(v) => Increment(v)
                case Toggle(v) => Increment(v)
                case ConditionalJump(c, o) => Copy(c, o)
                case Copy(c, o) => ConditionalJump(c, o)
              }
              this.copy(registers, this.pc + 1, instructions.updated(this.pc + offset, nextInstruction))
            }
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
            Copy(ss(1)(0), ss(2)(0))
          else
            Copy(ss(1).toInt, ss(2)(0))
        case "inc" => Increment(ss(1)(0))
        case "dec" => Decrement(ss(1)(0))
        case "jnz" =>
          if (ss(1)(0).isLetter && ss(2)(0).isLetter)
            ConditionalJump(ss(1)(0), ss(2)(0))
          else if (ss(1)(0).isLetter)
            ConditionalJump(ss(1)(0), ss(2).toInt)
          else if (ss(2)(0).isLetter)
            ConditionalJump(ss(1).toInt, ss(2)(0))
          else
            ConditionalJump(ss(1).toInt, ss(2).toInt)
        case "tgl" =>
          if (ss(1)(0).isLetter)
            Toggle(ss(1)(0))
          else
            Toggle(ss(1).toInt)
      }
    }
    var program1 = Program(Map('a' -> 7, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0, instructions)
    while (!program1.halts) {
      program1 = program1.applyNext
    }
    var program2 = Program(Map('a' -> 12, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0, instructions)
    while (!program2.halts) {
      program2 = program2.applyNext
    }
    (program1.registers('a'), program2.registers('a'))
  }
}
