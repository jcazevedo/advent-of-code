package net.jcazevedo.adventofcode

class Day03 extends DailyChallenge[Int, Int] {
  def valid(s1: Int, s2: Int, s3: Int) =
    (s1 + s2 > s3) && (s1 + s3) > s2 && (s2 + s3) > s1

  def run(filename: String) = {
    val input = io.Source.fromFile(filename).getLines.toList
    val triangles = input.map(_.trim.split("\\s+").map(_.toInt)).map(x => (x(0), x(1), x(2)))
    val otherTriangles = {
      val lines = input.map(_.trim.split("\\s+").map(_.toInt))
      (lines.map(_(0)) ++ lines.map(_(1)) ++ lines.map(_(2))).sliding(3, 3).map(x => (x(0), x(1), x(2)))
    }
    (triangles.filter((valid _).tupled).size, otherTriangles.filter((valid _).tupled).size)
  }
}
