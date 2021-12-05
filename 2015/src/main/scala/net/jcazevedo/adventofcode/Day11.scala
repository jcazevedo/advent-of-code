package net.jcazevedo.adventofcode

class Day11 extends DailyChallenge[String, String] {
  def run(filename: String): (String, String) = {
    val input = io.Source.fromFile(filename).getLines.mkString

    def nextChar(c: Char) = {
      if (c == 'z')
        'a'
      else
        (c.toInt + 1).toChar
    }

    def increment(s: String): String = {
      var next = s
      var i = 0
      while (true) {
        if (i >= s.size) {
          next += 'a'
          return next
        } else {
          next = next.updated(i, nextChar(next(i)))
          if (next(i) == 'a')
            i += 1
          else
            return next
        }
      }
      next
    }

    def valid(s: String) = {
      val req1 = s.sliding(3).exists { s =>
        s(2).toInt == s(1).toInt + 1 && s(1).toInt == s(0).toInt + 1
      }
      val req2 = s.indexOf('i') == -1 && s.indexOf('o') == -1 && s.indexOf('l') == -1
      val req3 = ('a' to 'z').map { c =>
        val cc = c.toString * 2
        if (s.indexOf(cc) == -1) 0 else 1
      }.sum >= 2
      req1 && req2 && req3
    }

    val it = Iterator.iterate(input.reverse)(increment).map { s =>
      val rev = s.reverse
      rev -> valid(rev)
    }

    (it.drop(1).dropWhile(!_._2).next._1, it.dropWhile(!_._2).next._1)
  }
}
