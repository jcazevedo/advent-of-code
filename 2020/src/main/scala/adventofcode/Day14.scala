package adventofcode

import scala.annotation.tailrec

class Day14 extends DailyChallenge[Long, Long] {
  sealed trait Instruction
  case class UpdateBitmask(mask: String) extends Instruction
  case class WriteValue(address: Long, value: Long) extends Instruction

  case class Mask(string: String) {
    val orV = string.foldLeft(0L) { case (acc, ch) => acc * 2L + (if (ch == '1') 1L else 0L) }
    val andV = string.foldLeft(0L) { case (acc, ch) => acc * 2L + (if (ch == '0') 0L else 1L) }

    def applyV(value: Long) = (value | orV) & andV

    def applyA(value: Long): List[Long] = {
      val bitRep = value.toBinaryString
      val orig = ("0" * (36 - bitRep.length)) + bitRep

      def go(original: List[Char], mask: List[Char], values: List[Long]): List[Long] = {
        ((original, mask): @unchecked) match {
          case (Nil, Nil)            => values
          case (oh :: ot, '0' :: mt) => go(ot, mt, values.map(x => x * 2L + (oh - '0')))
          case (_ :: ot, '1' :: mt)  => go(ot, mt, values.map(x => x * 2L + 1L))
          case (_ :: ot, 'X' :: mt)  => go(ot, mt, values.flatMap(x => List(0L, 1L).map(b => x * 2L + b)))
        }
      }

      go(orig.toList, string.toList, List(0L))
    }
  }

  case class State(mem: Map[Long, Long], mask: Mask)
  object State {
    val empty = State(Map.empty, Mask("X" * 36))
  }

  @tailrec
  private def run1(state: State, instructions: List[Instruction]): State =
    instructions match {
      case UpdateBitmask(newMask) :: t => run1(state.copy(mask = Mask(newMask)), t)
      case WriteValue(address, value) :: t =>
        run1(state.copy(mem = state.mem.updated(address, state.mask.applyV(value))), t)
      case Nil => state
    }

  @tailrec
  private def run2(state: State, instructions: List[Instruction]): State =
    instructions match {
      case UpdateBitmask(newMask) :: t => run2(state.copy(mask = Mask(newMask)), t)
      case WriteValue(address, value) :: t =>
        val newMem = state.mask.applyA(address).foldLeft(state.mem) { case (mem, addr) => mem.updated(addr, value) }
        run2(state.copy(mem = newMem), t)
      case Nil => state
    }

  def part1(instructions: List[Instruction]): Long = run1(State.empty, instructions).mem.values.sum

  def part2(instructions: List[Instruction]): Long = run2(State.empty, instructions).mem.values.sum

  def run(input: String): (Long, Long) = {
    val setBitmaskRegex = raw"""mask = ([01X]+)""".r
    val writeValueRegex = raw"""mem\[(\d+)\] = (\d+)""".r
    val instructions = input
      .split("\n")
      .map {
        case setBitmaskRegex(mask)           => UpdateBitmask(mask)
        case writeValueRegex(address, value) => WriteValue(address.toLong, value.toLong)
      }
      .toList
    (part1(instructions), part2(instructions))
  }
}
