package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day16 extends DailyChallenge[String, String] {
  def dragonCurve(s: String, length: Int): String = {
    if (s.length() >= length)
      s.take(length)
    else {
      dragonCurve(s + "0" + s.reverse.map {
        case '0' => '1'
        case '1' => '0'
        case other => other // shouldn't happen
      }, length)
    }
  }

  def checksum(s: String): String = {
    var res = mutable.ListBuffer[Char]()
    (0 until s.length() by 2).foreach { i =>
      res += (if (s(i) == s(i + 1)) '1' else '0')
    }
    if (res.length % 2 == 0)
      checksum(res.mkString)
    else
      res.mkString
  }

  def run(filename: String): (String, String) = {
    val length1 = 272
    val length2 = 35651584
    val state = io.Source.fromFile(filename).getLines.mkString
    (checksum(dragonCurve(state, length1)), checksum(dragonCurve(state, length2)))
  }
}
