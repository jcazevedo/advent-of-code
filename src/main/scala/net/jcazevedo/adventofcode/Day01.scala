package net.jcazevedo.adventofcode

class Day01 extends DailyChallenge[Int, Int] {
  case class Instruction(dir: Int)

  object Instruction {
    def gen(s: String): List[Instruction] = {
      val dir = if (s(0) == 'R') 1 else -1
      val size = s.drop(1).toInt
      Instruction(dir) :: (1 until size).map(_ => Instruction(0)).toList
    }
  }

  def run(filename: String) = {
    val input = io.Source.fromFile(filename).getLines.toList.mkString
    val instructions = input.split(",").map(_.trim).map(Instruction.gen).flatten
    val ((v, h), _, rep, _) = instructions.foldLeft(((0, 0), 0, None: Option[(Int, Int)], Set((0, 0)))) {
      case (((v, h), d, rep, vis), inst) =>
        val nextDir = ((d + inst.dir) + 4) % 4
        val s = if (nextDir < 2) 1 else -1
        val nextV = v + (if (nextDir % 2 == 0) s else 0)
        val nextH = h + (if (nextDir % 2 != 0) s else 0)
        val coord = (nextV, nextH)
        (coord, nextDir, if (rep.isEmpty && vis.contains(coord)) Some(coord) else rep, vis + coord)
    }
    val dist = math.abs(v) + math.abs(h)
    val repDist = math.abs(rep.get._1) + math.abs(rep.get._2)
    (dist, repDist)
  }
}
