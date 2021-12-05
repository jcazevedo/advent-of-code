package net.jcazevedo.adventofcode

import java.security.MessageDigest
import scala.collection.mutable

class Day17 extends DailyChallenge[String, Int] {
  val WIDTH = 4
  val HEIGHT = 4
  val OPEN_CHARS = Set('b', 'c', 'd', 'e', 'f')
  val TARGET = (3, 3)

  val md5Instance = MessageDigest.getInstance("MD5")
  def md5(s: String): String = {
    md5Instance.digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  val dirs = Map(
    'U' -> ((0, -1)),
    'D' -> ((0, 1)),
    'L' -> ((-1, 0)),
    'R' -> ((1, 0)))

  def shortest(start: (Int, Int), passcode: String): String = {
    val q = mutable.Queue[((Int, Int), String)]()
    q += ((start, ""))

    while (!q.isEmpty) {
      val (curr, path) = q.dequeue()

      if (curr == TARGET)
        return path

      val hash = md5(passcode + path)
      val (x, y) = curr

      dirs.zipWithIndex.foreach {
        case ((d, (dx, dy)), i) =>
          if (x + dx >= 0 && x + dx < WIDTH && y + dy >= 0 && y + dy < HEIGHT && OPEN_CHARS.contains(hash(i)))
            q += (((x + dx, y + dy), path + d))
      }
    }

    ""
  }

  def longest(start: (Int, Int), passcode: String): Int = {
    val q = mutable.Queue[((Int, Int), String)]()
    q += ((start, ""))

    var best = 0

    while (!q.isEmpty) {
      val (curr, path) = q.dequeue()

      if (curr == TARGET)
        best = path.length
      else {
        val hash = md5(passcode + path)
        val (x, y) = curr

        dirs.zipWithIndex.foreach {
          case ((d, (dx, dy)), i) =>
            if (x + dx >= 0 && x + dx < WIDTH && y + dy >= 0 && y + dy < HEIGHT && OPEN_CHARS.contains(hash(i)))
              q += (((x + dx, y + dy), path + d))
        }
      }
    }

    best
  }

  def run(filename: String): (String, Int) = {
    val passcode = io.Source.fromFile(filename).getLines.mkString
    (shortest((0, 0), passcode), longest((0, 0), passcode))
  }
}
