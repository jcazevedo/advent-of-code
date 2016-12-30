package net.jcazevedo.adventofcode

class Day21 extends DailyChallenge[String, String] {
  sealed trait Instruction
  case class SwapPosition(x: Int, y: Int) extends Instruction
  case class SwapLetter(x: Char, y: Char) extends Instruction
  case class RotateLeft(steps: Int) extends Instruction
  case class RotateRight(steps: Int) extends Instruction
  case class RotateBased(x: Char) extends Instruction
  case class ReversePositions(x: Int, y: Int) extends Instruction
  case class MovePosition(x: Int, y: Int) extends Instruction

  def applyInstruction(s: String, i: Instruction): String = {
    i match {
      case SwapPosition(x, y) => s.updated(x, s(y)).updated(y, s(x))
      case SwapLetter(x, y) => {
        val ix = s.indexOf(x)
        val iy = s.indexOf(y)
        s.updated(ix, s(iy)).updated(iy, s(ix))
      }
      case RotateLeft(steps) => (0 until steps).foldLeft(s) {
        case (s, _) =>
          s.drop(1) + s.head
      }
      case RotateRight(steps) => (0 until steps).foldLeft(s) {
        case (s, _) =>
          s.last + s.dropRight(1)
      }
      case RotateBased(x) => {
        val i = s.indexOf(x)
        val steps = i + 1 + (if (i >= 4) 1 else 0)
        (0 until steps).foldLeft(s) {
          case (s, _) =>
            s.last + s.dropRight(1)
        }
      }
      case ReversePositions(x, y) => {
        s.take(x) + s.substring(x, y + 1).reverse + s.drop(y + 1)
      }
      case MovePosition(x, y) => {
        val ch = s(x)
        val rem = s.take(x) + s.drop(x + 1)
        rem.take(y) + ch + rem.drop(y)
      }
    }
  }

  def applyInstructions(s: String, instructions: Seq[Instruction]): String = {
    instructions.foldLeft(s) {
      case (pwd, inst) =>
        applyInstruction(pwd, inst)
    }
  }

  def run(filename: String): (String, String) = {
    val instructions = io.Source.fromFile(filename).getLines.toList.map { instruction =>
      val ss = instruction.split("\\s+")
      if (ss(0) == "swap" && ss(1) == "position")
        SwapPosition(ss(2).toInt, ss(5).toInt)
      else if (ss(0) == "swap" && ss(1) == "letter")
        SwapLetter(ss(2)(0), ss(5)(0))
      else if (ss(0) == "rotate" && ss(1) == "left")
        RotateLeft(ss(2).toInt)
      else if (ss(0) == "rotate" && ss(1) == "right")
        RotateRight(ss(2).toInt)
      else if (ss(0) == "rotate" && ss(1) == "based")
        RotateBased(ss(6)(0))
      else if (ss(0) == "reverse")
        ReversePositions(ss(2).toInt, ss(4).toInt)
      else
        MovePosition(ss(2).toInt, ss(5).toInt)
    }
    val start = "abcdefgh"
    val res = applyInstructions(start, instructions)
    val perm = start.permutations.buffered.dropWhile(applyInstructions(_, instructions) != "fbgdceah")
    (res, perm.next)
  }
}
