package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day03 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def it1 = {
      var c = 1
      var l = 0
      var h = 0
      var n = 1
      var s = +1
      var len = 0
      def aux = {
        val res = len
        n += 1
        if (n > c * c) {
          c += 2
          l += 1
          h += 2
          s = -1
          len += 1
        } else {
          len += s
        }
        if (len == l)
          s = +1
        else if (len == h)
          s = -1
        res
      }
      Iterator.continually(aux)
    }

    def it2 = {
      var c = 1
      var n = 1
      var i = 0
      var j = 0
      var di = -1
      var dj = 0
      val m = mutable.Map[(Int, Int), Int]()
      def neighbors = {
        (-1 to 1).toList.flatMap { di =>
          (-1 to 1).toList.map { dj =>
            m.getOrElse((i + di, j + dj), 0)
          }
        }.sum
      }
      def aux = {
        val s = if (n == 1)
          1
        else
          neighbors
        m((i, j)) = s
        n += 1
        if (n > c * c) {
          c += 2
          j += 1
          di = -1
          dj = 0
        } else {
          val t = (c - 1) / 2
          if (i == -t && j != -t) {
            di = 0
            dj = -1
          } else if (j == -t && i != t) {
            di = 1
            dj = 0
          } else if (i == t && j != t) {
            di = 0
            dj = 1
          } else if (j == t) {
            di = -1
            dj = 0
          }
          i += di
          j += dj
        }
        s
      }
      Iterator.continually(aux)
    }

    val res1 = it1.drop(347990).next
    val res2 = it2.buffered.dropWhile(_ <= 347991).next
    (res1, res2)
  }
}
