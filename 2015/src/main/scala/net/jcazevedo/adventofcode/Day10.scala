package net.jcazevedo.adventofcode

class Day10 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val input = io.Source.fromFile(filename).getLines.mkString

    def split(s: String, res: Vector[String] = Vector()): Vector[String] = {
      s match {
        case "" => res
        case other =>
          val h = s(0)
          val seg = s.takeWhile(_ == h)
          split(s.drop(seg.length), res :+ seg)
      }
    }

    def lookAndSay(s: String): String = {
      split(s).map { s =>
        s.size.toString + s(0).toString
      }.mkString
    }

    val res = (0 until 40).foldLeft(input) { (s, _) =>
      lookAndSay(s)
    }

    val res1 = (0 until 10).foldLeft(res) { (s, _) =>
      lookAndSay(s)
    }

    (res.size, res1.size)
  }
}
