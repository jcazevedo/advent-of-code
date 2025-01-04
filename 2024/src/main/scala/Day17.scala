import scala.collection.mutable

object Day17 extends DailyChallenge[String, Long] {
  case class CPU(registers: Map[Char, Long], opcodes: Vector[Int])

  def pow(base: Long, exponent: Long): Long =
    if (exponent == 0L) 1L
    else {
      val tmp = pow(base, exponent / 2)
      (if (exponent % 2 == 1) base else 1) * tmp * tmp
    }

  def run(cpu: CPU): Vector[Int] = {
    val outputs = mutable.ListBuffer.empty[Int]
    var ip = 0
    val registers = mutable.Map[Char, Long](cpu.registers.toSeq*)

    def operand(pointer: Int, combo: Boolean): Long =
      if (combo) cpu.opcodes(pointer) match {
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => 3
        case 4 => registers('A')
        case 5 => registers('B')
        case 6 => registers('C')
      }
      else cpu.opcodes(pointer)

    while (ip >= 0 && ip < cpu.opcodes.length) {
      cpu.opcodes(ip) match {
        case 0 =>
          registers('A') = registers('A') / pow(2L, operand(ip + 1, combo = true))
          ip += 2
        case 1 =>
          registers('B') = registers('B') ^ operand(ip + 1, combo = false)
          ip += 2
        case 2 =>
          registers('B') = operand(ip + 1, combo = true) % 8
          ip += 2
        case 3 =>
          if (registers('A') == 0) ip += 2
          else ip = operand(ip + 1, combo = false).toInt
        case 4 =>
          registers('B') = registers('B') ^ registers('C')
          ip += 2
        case 5 =>
          outputs.addOne((operand(ip + 1, combo = true) % 8).toInt)
          ip += 2
        case 6 =>
          registers('B') = registers('A') / pow(2L, operand(ip + 1, combo = true))
          ip += 2
        case 7 =>
          registers('C') = registers('A') / pow(2L, operand(ip + 1, combo = true))
          ip += 2
      }
    }

    outputs.toVector
  }

  def find(remainingOpcodes: Vector[Int], curr: Long = 0L): Long = {
    if (remainingOpcodes.isEmpty) curr
    else {
      var a = -1L
      var b = -1L
      var c = -1L

      var at = 0
      while (at < 8) {
        // The input program is as follows:
        //
        // B = A % 8
        // B = B xor 3
        // C = A / (2 ^ B)
        // B = B xor C
        // A = A / (2 ^ 3)
        // B = B xor 5
        // print(B)
        // if (A (0) != 0) repeat
        a = at + curr * 8
        b = a % 8
        b = b ^ 3
        c = a / pow(2, b)
        b = b ^ c
        b = b ^ 5
        if (b % 8 == remainingOpcodes.last) {
          val next = find(remainingOpcodes.init, curr * 8 + at)
          if (next != -1) return next
        }
        at += 1
      }

      -1
    }
  }

  def run(input: String): (String, Long) = {
    val cpuRegex = """Register A: ([0-9]+)\nRegister B: ([0-9]+)\nRegister C: ([0-9]+)\n\nProgram: (.*)""".r

    val cpu = cpuRegex
      .findAllIn(input)
      .collect({ case cpuRegex(a, b, c, opcodes) =>
        CPU(Map('A' -> a.toInt, 'B' -> b.toInt, 'C' -> c.toInt), opcodes.split(",").map(_.toInt).toVector)
      })
      .next

    val part1 = run(cpu).mkString(",")
    val part2 = find(cpu.opcodes)

    (part1, part2)
  }
}
