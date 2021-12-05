package net.jcazevedo.adventofcode

class Day13 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    case class Scanner(range: Int, position: Int, direction: Int = -1) {
      def move: Scanner = {
        val nextDir = if (position == (range - 1) || position == 0) -direction else direction
        this.copy(position = position + nextDir, direction = nextDir)
      }
    }

    val lines = io.Source.fromFile(filename).getLines().toList

    val scanners: Map[Int, Scanner] = lines.map { l =>
      val Array(depth, range) = l.split(":")
      depth.trim.toInt -> Scanner(range.trim.toInt, 0)
    }.toMap

    def moveAll(scanners: Map[Int, Scanner]): Map[Int, Scanner] =
      scanners.map { case (k, v) => k -> v.move }

    val maxDepth = scanners.keySet.max

    def severity(from: Int, to: Int, scanners: Map[Int, Scanner]): Int =
      if (from > to)
        0
      else
        scanners.get(from) match {
          case None => severity(from + 1, to, moveAll(scanners))
          case Some(scanner) =>
            val currSeverity = if (scanner.position == 0) from * scanner.range else 0
            currSeverity + severity(from + 1, to, moveAll(scanners))
        }

    def gotCaught(from: Int, to: Int, scanners: Map[Int, Scanner]): Boolean =
      if (from > to)
        false
      else scanners.get(from) match {
        case Some(scanner) if scanner.position == 0 => true
        case _ => gotCaught(from + 1, to, moveAll(scanners))
      }

    def it = Iterator.iterate((0, scanners)) { case (n, scanners) => (n + 1, moveAll(scanners)) }

    def sols = it.map {
      case (n, initialScanners) =>
        n -> gotCaught(0, maxDepth, initialScanners)
    }

    (severity(0, maxDepth, scanners), sols.dropWhile(_._2).next._1)
  }
}
