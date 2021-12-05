package net.jcazevedo.adventofcode

class Day17 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    case class Node(value: Int, var next: Node)

    def rotate(l: List[Int], times: Int): List[Int] =
      if (times % l.size == 0)
        l
      else {
        val t = times % l.size
        l.drop(t) ++ l.take(t)
      }

    def insert(maxValue: Int, steps: Int): Iterator[Int] = {
      val l = (1 to maxValue).foldLeft(List[Int](0)) {
        case (l, v) =>
          rotate(l, steps) :+ v
      }
      Iterator.continually(l).flatten
    }

    val steps = io.Source.fromFile(filename).getLines().next().toInt

    val v1 = insert(2017, steps).dropWhile(_ != 2017).drop(1).next

    var l = 1
    var i = 0
    var res = 0
    (1 to 50000000).foreach { next =>
      i = (i + steps) % l
      i += 1
      if (i == 1)
        res = next
      l += 1
    }

    (v1, res)
  }
}
