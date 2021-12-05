package net.jcazevedo.adventofcode

class Day09 extends DailyChallenge[Int, Long] {
  def decompress(s: String): String = {
    var res = ""
    var repeat = ""
    var left = -1
    var right = -1
    var inFetchRepeatSection = false
    (0 until s.length).foreach { i =>
      if (s(i) == '(' && left == -1 && right == -1) {
        left = 0
      } else if (s(i) == 'x' && left != -1 && right == -1) {
        right = 0
      } else if (s(i) == ')' && left != -1 && right != -1 && !inFetchRepeatSection) {
        inFetchRepeatSection = true
      } else {
        if (left != -1 && right == -1)
          left = left * 10 + (s(i) - '0')
        else if (right != -1 && !inFetchRepeatSection)
          right = right * 10 + (s(i) - '0')
        else if (inFetchRepeatSection) {
          repeat += s(i)
          left -= 1
          if (left == 0) {
            res += (repeat * right)
            left = -1
            right = -1
            inFetchRepeatSection = false
            repeat = ""
          }
        } else
          res += s(i)
      }
    }
    if (right > 0)
      res += (repeat * right)
    res
  }

  def decompressLength(s: String): Long = {
    var repeat = ""
    var left = -1
    var right = -1
    var inFetchRepeatSection = false
    var tot = 0l
    (0 until s.length).foreach { i =>
      if (s(i) == '(' && left == -1 && right == -1) {
        left = 0
      } else if (s(i) == 'x' && left != -1 && right == -1) {
        right = 0
      } else if (s(i) == ')' && left != -1 && right != -1 && !inFetchRepeatSection) {
        inFetchRepeatSection = true
      } else {
        if (left != -1 && right == -1)
          left = left * 10 + (s(i) - '0')
        else if (right != -1 && !inFetchRepeatSection)
          right = right * 10 + (s(i) - '0')
        else if (inFetchRepeatSection) {
          repeat += s(i)
          left -= 1
          if (left == 0) {
            tot += decompressLength(repeat) * right
            left = -1
            right = -1
            inFetchRepeatSection = false
            repeat = ""
          }
        } else
          tot += 1
      }
    }
    tot
  }

  def run(filename: String): (Int, Long) = {
    val input = io.Source.fromFile(filename).getLines.toList.mkString
    val dec = decompress(input)
    (dec.length, decompressLength(input))
  }
}
