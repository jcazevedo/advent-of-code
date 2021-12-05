package net.jcazevedo.adventofcode

class Day16 extends DailyChallenge[String, String] {
  sealed trait Instruction
  case class Spin(x: Int) extends Instruction
  case class Exchange(a: Int, b: Int) extends Instruction
  case class Partner(a: Char, b: Char) extends Instruction

  def run(filename: String): (String, String) = {
    def apply(programs: String, instruction: Instruction): String =
      instruction match {
        case Spin(x) => programs.takeRight(x) ++ programs.dropRight(x)
        case Exchange(a, b) => programs.updated(a, programs(b)).updated(b, programs(a))
        case Partner(a, b) => programs.updated(programs.indexOf(a), b).updated(programs.indexOf(b), a)
      }

    val programs: String = (0 until 16).map(_ + 'a').map(_.toChar).mkString

    val lines = io.Source.fromFile(filename).getLines().toList
    val instructions: Seq[Instruction] = lines.head.split(",").map { instruction =>
      if (instruction.startsWith("s"))
        Spin(instruction.drop(1).toInt)
      else if (instruction.startsWith("x")) {
        val Array(a, b) = instruction.drop(1).split("/")
        Exchange(a.toInt, b.toInt)
      } else {
        val Array(a, b) = instruction.drop(1).split("/")
        Partner(a.trim.head, b.trim.head)
      }
    }

    val rep = 1000000000

    def progIterator(programs: String) = new Iterator[String] {
      val instrs = Iterator.continually(instructions).flatten
      var current = programs

      def hasNext = true

      def next(): String = {
        val res = current
        current = apply(current, instrs.next())
        res
      }
    }

    val cycle = progIterator(programs).drop(1).takeWhile(_ != programs).toList.size + 1
    val p1 = instructions.foldLeft(programs)(apply)
    val p2 = progIterator(programs).drop(rep % cycle).next()

    (p1, p2)
  }
}
