package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day08 extends DailyChallenge[Long, Long] {
  def run(filename: String): (Long, Long) = {
    val lines = io.Source.fromFile(filename).getLines.toList
    val registers = mutable.Map[String, Long]().withDefaultValue(0l)
    def cond(register: String, op: String, value: Long): Boolean =
      op match {
        case ">" => registers(register) > value
        case ">=" => registers(register) >= value
        case "<" => registers(register) < value
        case "<=" => registers(register) <= value
        case "==" => registers(register) == value
        case "!=" => registers(register) != value
        case other => throw new IllegalArgumentException(s"Unknown conditional operator $other")
      }

    def op(register: String, op: String, value: Long): Unit =
      op match {
        case "inc" => registers(register) = registers(register) + value
        case "dec" => registers(register) = registers(register) - value
        case other => throw new IllegalArgumentException(s"Unknown operator $other")
      }

    val highestValues = mutable.ListBuffer[Long]()

    lines.foreach { line =>
      val Array(reg1, op1, v1, _, reg2, op2, v2) = line.split("\\s+")
      if (cond(reg2, op2, v2.toLong))
        op(reg1, op1, v1.toLong)
      if (registers.nonEmpty)
        highestValues += registers.values.max
    }

    (registers.values.max, highestValues.max)
  }
}
