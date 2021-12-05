package net.jcazevedo.adventofcode

class Day02 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines.toList
    val ns = lines.map(_.split("\\s+").map(_.toInt))
    val res1 = ns.map { n =>
      val ss = n.sorted
      ss.last - ss.head
    }.sum
    val res2 = ns.map { n =>
      var l = 0
      n.zipWithIndex.foreach {
        case (n1, i) =>
          n.zipWithIndex.foreach {
            case (n2, j) =>
              if (i != j && n1 % n2 == 0)
                l = n1 / n2
          }
      }
      l
    }.sum
    (res1, res2)
  }
}
