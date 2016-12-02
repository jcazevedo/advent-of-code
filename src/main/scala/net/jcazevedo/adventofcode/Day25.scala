package net.jcazevedo.adventofcode

class Day25 extends DailyChallenge[Long, Long] {
  def index(r: Int, c: Int): Int = {
    val d = r + c - 1
    val c1 = (d - 1) * d / 2
    c1 + c - 1
  }

  def next(l: Long): Long = {
    (l * 252533) % 33554393
  }

  def run(filename: String): (Long, Long) = {
    val splits = io.Source.fromFile(filename).getLines.mkString.split("\\s")
    val r = splits(16).dropRight(1).toInt
    val c = splits(18).dropRight(1).toInt
    val i = index(r, c)
    val t = Iterator.iterate(20151125l)(next).drop(i).next

    (t, -1)
  }
}
