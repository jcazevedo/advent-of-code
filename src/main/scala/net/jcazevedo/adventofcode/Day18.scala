package net.jcazevedo.adventofcode

import scala.collection.immutable.Queue
import scala.util.Try

class Day18 extends DailyChallenge[Long, Long] {
  case class Registers(values: Map[String, Long] = Map()) {
    def updated(x: String, v: Long) = this.copy(values = values.updated(x, v))
  }

  case class Program1(registers: Registers = Registers(), pc: Int = 0, lastSound: Option[Long] = None) {
    def valueOf(x: String): Long = Try(x.toLong).toOption.getOrElse(registers.values.getOrElse(x, 0l))
  }

  case class InnerProgram2(registers: Registers = Registers(), pc: Int = 0, queue: Queue[Long] = Queue()) {
    def done(instructions: IndexedSeq[Instruction]): Boolean =
      pc < 0 || pc >= instructions.length
    def waiting(instructions: IndexedSeq[Instruction]): Boolean =
      !done(instructions) && instructions(pc).isInstanceOf[RCV] && queue.isEmpty
    def valueOf(x: String): Long = Try(x.toLong).toOption.getOrElse(registers.values.getOrElse(x, 0l))
  }

  case class Program2(programs: Vector[InnerProgram2], current: Int = 0) {
    def isDeadlock(instructions: IndexedSeq[Instruction]): Boolean =
      programs.forall(p => p.done(instructions) || p.waiting(instructions))

    def currentProg: InnerProgram2 = programs(current)
    def switch = this.copy(current = current ^ 1)
  }

  sealed trait Instruction {
    def exec(prog: Program1): Program1
    def exec(prog: Program2): Program2
  }

  case class SND(x: String) extends Instruction {
    def exec(prog: Program1) =
      prog.copy(pc = prog.pc + 1, lastSound = Some(prog.valueOf(x)))

    def exec(prog: Program2) = {
      val current = prog.current
      val other = current ^ 1
      val currentProg = prog.programs(current)
      val otherProg = prog.programs(other)
      prog.copy(programs =
        prog.programs
          .updated(current, currentProg.copy(pc = currentProg.pc + 1))
          .updated(other, otherProg.copy(queue = otherProg.queue.enqueue(currentProg.valueOf(x)))))
    }
  }

  case class SET(x: String, y: String) extends Instruction {
    def exec(prog: Program1) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(y)))

    def exec(prog: Program2) = {
      val current = prog.current
      val currentProg = prog.programs(current)
      prog.copy(programs =
        prog.programs.updated(
          current,
          currentProg.copy(pc = currentProg.pc + 1, registers = currentProg.registers.updated(x, currentProg.valueOf(y)))))
    }
  }

  case class ADD(x: String, y: String) extends Instruction {
    def exec(prog: Program1) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(x) + prog.valueOf(y)))

    def exec(prog: Program2) = {
      val current = prog.current
      val currentProg = prog.programs(current)
      prog.copy(programs =
        prog.programs.updated(
          current,
          currentProg.copy(pc = currentProg.pc + 1, registers = currentProg.registers.updated(x, currentProg.valueOf(x) + currentProg.valueOf(y)))))
    }
  }

  case class MUL(x: String, y: String) extends Instruction {
    def exec(prog: Program1) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(x) * prog.valueOf(y)))

    def exec(prog: Program2) = {
      val current = prog.current
      val currentProg = prog.programs(current)
      prog.copy(programs =
        prog.programs.updated(
          current,
          currentProg.copy(pc = currentProg.pc + 1, registers = currentProg.registers.updated(x, currentProg.valueOf(x) * currentProg.valueOf(y)))))
    }
  }

  case class MOD(x: String, y: String) extends Instruction {
    def exec(prog: Program1) =
      prog.copy(pc = prog.pc + 1, registers = prog.registers.updated(x, prog.valueOf(x) % prog.valueOf(y)))

    def exec(prog: Program2) = {
      val current = prog.current
      val currentProg = prog.programs(current)
      prog.copy(programs =
        prog.programs.updated(
          current,
          currentProg.copy(pc = currentProg.pc + 1, registers = currentProg.registers.updated(x, currentProg.valueOf(x) % currentProg.valueOf(y)))))
    }
  }

  case class RCV(x: String) extends Instruction {
    def exec(prog: Program1) = prog

    def exec(prog: Program2) = {
      val current = prog.current
      val currentProg = prog.programs(current)
      prog.copy(programs =
        prog.programs.updated(
          current,
          currentProg.copy(pc = currentProg.pc + 1, registers = currentProg.registers.updated(x, currentProg.queue.head), queue = currentProg.queue.tail)))
    }
  }

  case class JGZ(x: String, y: String) extends Instruction {
    def exec(prog: Program1) =
      prog.copy(pc = prog.pc + (if (prog.valueOf(x) > 0) prog.valueOf(y).toInt else 1))

    def exec(prog: Program2) = {
      val current = prog.current
      val currentProg = prog.programs(current)
      prog.copy(programs =
        prog.programs.updated(
          current,
          currentProg.copy(pc = currentProg.pc + (if (currentProg.valueOf(x) > 0) currentProg.valueOf(y).toInt else 1))))
    }
  }

  private def run(prog: Program1, instructions: IndexedSeq[Instruction]): Option[Option[Long]] = {
    if (prog.pc < 0 || prog.pc >= instructions.length)
      None
    else {
      val current = instructions(prog.pc)
      current match {
        case RCV(x) if prog.valueOf(x) != 0 => Some(prog.lastSound)
        case other =>
          run(other.exec(prog), instructions)
      }
    }
  }

  private def run(prog: Program2, instructions: IndexedSeq[Instruction], nSends: Long = 0): Long = {
    if (prog.isDeadlock(instructions))
      nSends
    else if (prog.currentProg.waiting(instructions))
      run(prog.switch, instructions, nSends)
    else {
      val n = if (prog.current == 1 && instructions(prog.currentProg.pc).isInstanceOf[SND]) 1 else 0
      val instruction = instructions(prog.currentProg.pc)
      run(instruction.exec(prog), instructions, nSends + n)
    }
  }

  def run(filename: String): (Long, Long) = {
    val lines = io.Source.fromFile(filename).getLines().toList
    val instructions = lines.map { line =>
      val ss = line.split(" ")
      ss(0) match {
        case "snd" => SND(ss(1))
        case "set" => SET(ss(1), ss(2))
        case "add" => ADD(ss(1), ss(2))
        case "mul" => MUL(ss(1), ss(2))
        case "mod" => MOD(ss(1), ss(2))
        case "rcv" => RCV(ss(1))
        case "jgz" => JGZ(ss(1), ss(2))
      }
    }.toIndexedSeq

    val res1 = run(Program1(), instructions).get.get
    val res2 = run(Program2(programs = Vector(InnerProgram2(Registers(Map("p" -> 0))), InnerProgram2(Registers(Map("p" -> 1))))), instructions)

    (res1, res2)
  }
}
