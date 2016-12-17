package net.jcazevedo.adventofcode

import java.security.MessageDigest

class Day05 extends DailyChallenge[String, String] {
  val md5Instance = MessageDigest.getInstance("MD5")

  def md5(s: String): String = {
    md5Instance.digest(s.getBytes).map("%02X".format(_)).mkString
  }

  def hashes(doorId: String) = Stream.from(0).map(i => md5(doorId + i)).filter(_.startsWith("00000"))

  def run(filename: String): (String, String) = {
    val doorId = io.Source.fromFile(filename).getLines.toList.mkString
    val passw = hashes(doorId).map(_.charAt(5)).take(8).toList.mkString.toLowerCase
    var str = "________"
    hashes(doorId).map(v => (v.charAt(5), v.charAt(6)))
      .filter(v => v._1 >= '0' && v._1 <= '7').map {
        case (idx, ch) =>
          val i = idx.toInt - '0'
          if (str.charAt(i) == '_') {
            str = str.substring(0, i) + ch + str.substring(i + 1)
            println(str)
          }
          str
      }.takeWhile(_.exists(_ == '_')).toList
    (passw, str.toLowerCase)
  }
}
