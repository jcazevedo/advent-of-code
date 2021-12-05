package net.jcazevedo.adventofcode

import scala.util.Try

class Day23 extends DailyChallenge[Int, Int] {
  case class Registers(values: Map[String, Long] = Map()) {
    def updated(x: String, v: Long) = this.copy(values = values.updated(x, v))
  }

  case class Program(registers: Registers = Registers(), pc: Int = 0, lastSound: Option[Long] = None) {
    def valueOf(x: String): Long = Try(x.toLong).toOption.getOrElse(registers.values.getOrElse(x, 0l))
  }

  sealed trait Instruction {
    def exec(prog: Program): Program
  }

  case class SET(x: String, y: String) extends Instruction {
    def exec(prog: Program) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(y)))
  }

  case class SUB(x: String, y: String) extends Instruction {
    def exec(prog: Program) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(x) - prog.valueOf(y)))
  }

  case class MUL(x: String, y: String) extends Instruction {
    def exec(prog: Program) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(x) * prog.valueOf(y)))
  }

  case class JNZ(x: String, y: String) extends Instruction {
    def exec(prog: Program) =
      prog.copy(pc = prog.pc + (if (prog.valueOf(x) != 0) prog.valueOf(y).toInt else 1))
  }

  private def run(prog: Program, instructions: IndexedSeq[Instruction], mulCount: Int = 0): Int = {
    if (prog.pc < 0 || prog.pc >= instructions.length)
      mulCount
    else {
      val current = instructions(prog.pc)
      current match {
        case inst: MUL =>
          run(inst.exec(prog), instructions, mulCount + 1)
        case other =>
          run(other.exec(prog), instructions, mulCount)
      }
    }
  }

  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines().toList
    val instructions = lines.map { line =>
      val ss = line.split(" ")
      ss(0) match {
        case "set" => SET(ss(1), ss(2))
        case "sub" => SUB(ss(1), ss(2))
        case "mul" => MUL(ss(1), ss(2))
        case "jnz" => JNZ(ss(1), ss(2))
      }
    }.toIndexedSeq

    val res1 = run(Program(), instructions)

    // The program is counting the number of non-primes between 109300 and 126300, by steps of 17.
    val start = 109300
    val end = 126300
    def isPrime(i: Int): Boolean =
      (2 to math.sqrt(i).toInt).count(i % _ == 0) == 0
    val res2 = (start to end by 17).count(!isPrime(_))

    (res1, res2)
  }
}
