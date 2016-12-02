package net.jcazevedo.adventofcode

class Day17 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val containers = io.Source.fromFile(filename).getLines.toList.map(_.toInt).toIndexedSeq
    val nContainers = containers.size

    def containersComb(totContainers: Int, i: Int = 0): List[List[Int]] = {
      if (i == totContainers)
        List(Nil)
      else {
        val rem = containersComb(totContainers, i + 1)
        rem.map { i :: _ } ++ rem
      }
    }

    val combs = containersComb(nContainers)
    val positives1 = combs.filter { c => c.map(containers).sum == 150 }
    val positivesWithSize =
      combs.map(c => (c.map(containers).sum, c.size)).filter(_._1 == 150)
    val minContainers = positivesWithSize.map(_._2).min
    val positives2 = positivesWithSize.filter(_._2 == minContainers)

    (positives1.size, positives2.size)
  }
}
