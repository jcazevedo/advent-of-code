package net.jcazevedo.adventofcode

class Day05 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines.toList
    val l = Array(lines.map(_.toInt): _*)
    val L = l.length
    var i = 0
    var steps = 0
    while (i >= 0 && i < L) {
      val pi = l(i)
      l(i) += 1
      i += pi
      steps += 1
    }
    val res1 = steps

    val nl = Array(lines.map(_.toInt): _*)
    i = 0
    steps = 0
    while (i >= 0 && i < L) {
      val pi = nl(i)
      if (pi >= 3)
        nl(i) -= 1
      else
        nl(i) += 1
      i += pi
      steps += 1
    }
    val res2 = steps
    (res1, res2)
  }
}
