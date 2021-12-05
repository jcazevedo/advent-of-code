package net.jcazevedo.adventofcode

class Day01 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines.toList
    val sequence = lines.head.map(_ - '0')
    def sumIfEqual(current: Int, pair: (Int, Int)) = pair match {
      case (i, j) if i == j => current + i
      case _ => current
    }
    val res1 = sequence.zip(sequence.tail :+ sequence.head).foldLeft(0)(sumIfEqual)
    val res2 = sequence.zip(sequence.drop(sequence.length / 2) ++ sequence.take(sequence.length / 2)).foldLeft(0)(sumIfEqual)
    (res1, res2)
  }
}
