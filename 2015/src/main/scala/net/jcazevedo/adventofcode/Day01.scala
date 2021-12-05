package net.jcazevedo.adventofcode

class Day01 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def floors(s: String): Seq[(Char, Int)] = {
      s.foldLeft((Seq[(Char, Int)](), 0)) {
        case ((s, floor), char) =>
          val nextFloor = char match {
            case '(' => floor + 1
            case ')' => floor - 1
            case _ => floor
          }
          (s :+ (char, nextFloor), nextFloor)
      }._1
    }

    def getFinalFloor(s: String): Int = {
      floors(s).last._2
    }

    def getBasementIndex(s: String): Int = {
      floors(s).zipWithIndex.dropWhile(_._1._2 != -1).head._2 + 1
    }

    val line = io.Source.fromFile(filename).getLines.toList.mkString
    val finalFloor = getFinalFloor(line)
    val basementIndex = getBasementIndex(line)

    (finalFloor, basementIndex)
  }
}
