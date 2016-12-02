package net.jcazevedo.adventofcode

import java.security.MessageDigest

class Day04 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def md5(s: String): String = {
      MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString
    }

    val secretKey = io.Source.fromFile(filename).getLines.toList.mkString
    val p1 = Iterator.from(1, 1).map(n => (n, md5(secretKey + n.toString))).dropWhile(_._2.take(5) != "00000").next._1
    val p2 = Iterator.from(1, 1).map(n => (n, md5(secretKey + n.toString))).dropWhile(_._2.take(6) != "000000").next._1

    (p1, p2)
  }
}
