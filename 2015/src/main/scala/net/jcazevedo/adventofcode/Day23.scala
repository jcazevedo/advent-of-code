package net.jcazevedo.adventofcode

class Day23 extends DailyChallenge[Int, Int] {
  sealed trait Instruction
  case class HLF(register: String) extends Instruction
  case class TPL(register: String) extends Instruction
  case class INC(register: String) extends Instruction
  case class JMP(offset: Int) extends Instruction
  case class JIE(register: String, offset: Int) extends Instruction
  case class JIO(register: String, offset: Int) extends Instruction
  case object UNKNOWN extends Instruction

  case class Program(registers: Map[String, Int], pc: Int, instructions: IndexedSeq[Instruction]) {
    def step: Program = {
      val c = instructions(pc)
      c match {
        case HLF(r) => this.copy(registers = registers.updated(r, registers(r) / 2), pc = pc + 1)
        case TPL(r) => this.copy(registers = registers.updated(r, registers(r) * 3), pc = pc + 1)
        case INC(r) => this.copy(registers = registers.updated(r, registers(r) + 1), pc = pc + 1)
        case JMP(o) => this.copy(pc = pc + o)
        case JIE(r, o) => this.copy(pc = (if (registers(r) % 2 == 0) pc + o else pc + 1))
        case JIO(r, o) => this.copy(pc = (if (registers(r) == 1) pc + o else pc + 1))
        case UNKNOWN => this
      }
    }

    def finished: Boolean = pc < 0 || pc >= instructions.size
  }

  def run(filename: String): (Int, Int) = {
    val instructions = io.Source.fromFile(filename).getLines.toList.map { line =>
      val splits = line.split(" ")
      splits(0) match {
        case "hlf" => HLF(splits(1))
        case "tpl" => TPL(splits(1))
        case "inc" => INC(splits(1))
        case "jmp" => JMP(splits(1).toInt)
        case "jie" => JIE(splits(1).dropRight(1), splits(2).toInt)
        case "jio" => JIO(splits(1).dropRight(1), splits(2).toInt)
      }
    }.toIndexedSeq

    var p1 = Program(Map("a" -> 0, "b" -> 0), 0, instructions)
    while (!p1.finished)
      p1 = p1.step

    var p2 = Program(Map("a" -> 1, "b" -> 0), 0, instructions)
    while (!p2.finished)
      p2 = p2.step

    (p1.registers("b"), p2.registers("b"))
  }
}
