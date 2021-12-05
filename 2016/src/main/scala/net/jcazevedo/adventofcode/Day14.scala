package net.jcazevedo.adventofcode

import java.security.MessageDigest
import scala.collection.mutable

class Day14 extends DailyChallenge[Int, Int] {
  val md5Instance = MessageDigest.getInstance("MD5")

  def md5(s: String): String = {
    md5Instance.digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  val cache = mutable.Map[(String, Int), String]()

  def hashes(salt: String, f: Int = 0, md5Calls: Int = 1): Stream[(Int, String)] =
    Stream.from(f).map(i =>
      (i,
        cache.getOrElseUpdate((salt + i, md5Calls),
          (0 until md5Calls).foldLeft(salt + i) { case (hash, _) => md5(hash) })))

  def keys(salt: String, md5Calls: Int = 1): Stream[(Int, String)] = {
    val h = hashes(salt, 0, md5Calls)
    h.filter {
      case (idx, hash) =>
        val triplets = mutable.ListBuffer[String]()
        (0 to hash.size - 3).foreach { idx =>
          if (hash(idx) == hash(idx + 1) && hash(idx) == hash(idx + 2))
            triplets += (hash(idx).toString * 3)
        }
        if (triplets.size > 0) {
          val next1000 = hashes(salt, idx + 1, md5Calls).take(1000).toList
          val target = triplets(0)(0).toString * 5
          next1000.exists {
            case (_, h) =>
              h.contains(target)
          }
        } else {
          false
        }
    }
  }

  def run(filename: String): (Int, Int) = {
    val salt = io.Source.fromFile(filename).getLines.mkString
    (keys(salt, 1).drop(63).head._1, keys(salt, 2017).drop(63).head._1)
  }
}
