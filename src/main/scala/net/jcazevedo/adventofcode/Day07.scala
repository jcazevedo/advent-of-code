package net.jcazevedo.adventofcode

class Day07 extends DailyChallenge[Int, Int] {
  def sequences(ip: String): (List[String], List[String]) = {
    val (l1, l2, _) = {
      val (l1, l2, fin) = ip.foldLeft((List[String](), List[String](), "")) {
        case ((l1, l2, curr), ch) =>
          if (ch == '[') {
            if (curr != "")
              (l1 :+ curr, l2, "")
            else
              (l1, l2, "")
          } else if (ch == ']') {
            if (curr != "")
              (l1, l2 :+ curr, "")
            else
              (l1, l2, "")
          } else
            (l1, l2, curr + ch)
      }
      if (fin != "")
        (l1 :+ fin, l2, "")
      else
        (l1, l2, "")
    }
    (l1, l2)
  }

  def isAbba(s: String) = s.length == 4 && s(0) != s(1) && s.startsWith(s.substring(2).reverse)

  def supportTls(ip: String): Boolean = {
    val (l1, l2) = sequences(ip)
    l1.exists(_.sliding(4).exists(isAbba)) && !l2.exists(_.sliding(4).exists(isAbba))
  }

  def supportSsl(ip: String): Boolean = {
    val (l1, l2) = sequences(ip)
    val l1Seqs = l1.map(_.sliding(3)).flatten
    val l2Seqs = l2.map(_.sliding(3)).flatten
    l1Seqs.exists { s1 =>
      s1(0) != s1(1) && s1(0) == s1(2) && l2Seqs.exists { s2 =>
        s2(0) == s1(1) && s2(1) == s1(0) && s2(2) == s1(1)
      }
    }
  }

  def run(filename: String): (Int, Int) = {
    val ips = io.Source.fromFile(filename).getLines.toList
    (ips.count(supportTls), ips.count(supportSsl))
  }
}
