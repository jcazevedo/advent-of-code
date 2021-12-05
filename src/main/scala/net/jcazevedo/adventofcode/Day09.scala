package net.jcazevedo.adventofcode

class Day09 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines.toList
    var totalScore = 0
    var nonCancellable = 0
    var inGroup = 0
    var garbage = false
    var ignore = false
    lines.head.foreach { ch =>
      if (!ignore) {
        if (ch == '!') {
          ignore = true
        } else if (garbage) {
          if (ch == '>') {
            garbage = false
          } else {
            nonCancellable += 1
          }
        } else if (ch == '<') {
          garbage = true
        } else if (ch == '{') {
          inGroup += 1
        } else if (ch == '}') {
          totalScore += inGroup
          inGroup -= 1
        }
      } else {
        ignore = false
      }
    }
    (totalScore, nonCancellable)
  }
}
