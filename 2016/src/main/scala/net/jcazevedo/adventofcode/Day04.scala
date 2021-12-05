package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day04 extends DailyChallenge[Int, Int] {
  def valid(room: String): Boolean = {
    if (room.last != ']' || room.filter(_ == '[').length != 1)
      false
    else {
      val checksum = room.substring(room.indexOf('[') + 1, room.lastIndexOf(']'))
      if (checksum.size != 5)
        false
      else {
        val parts = room.substring(0, room.indexOf('[')).split("-")
        val names = parts.dropRight(1)
        val sectorId = parts.last
        if (names.exists(x => x.exists(ch => !ch.isLetter || ch > 'z')) || sectorId.exists(!_.isDigit))
          false
        else {
          val counts = names.flatten.foldLeft(Map[Char, Int]()) {
            case (map, ch) =>
              map.updated(ch, map.getOrElse(ch, 0) + 1)
          }
          val checks = counts.toList.sortBy(x => (-x._2, x._1)).take(5).map(_._1).mkString
          checks == checksum
        }
      }
    }
  }

  def sectorId(room: String): Int =
    room.substring(0, room.indexOf('[')).split("-").last.toInt

  def parts(room: String): List[String] =
    room.substring(0, room.indexOf('[')).split("-").dropRight(1).toList

  def rotate(word: String, times: Int): String = {
    val alph = 'z' - 'a' + 1
    word.map { ch =>
      ((ch - 'a' + times) % alph) + 'a'
    }.map(_.toChar).mkString
  }

  def run(filename: String): (Int, Int) = {
    val rooms = io.Source.fromFile(filename).getLines.toList
    val validRooms = rooms.filter(valid)
    val checksums = validRooms.map(sectorId)
    val room = validRooms.find { room =>
      val p = parts(room)
      val s = sectorId(room)
      p.map(rotate(_, s)).mkString(" ") == "northpole object storage"
    }
    (checksums.sum, sectorId(room.get))
  }
}
