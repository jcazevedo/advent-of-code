package net.jcazevedo.adventofcode

class Day06 extends DailyChallenge[String, String] {
  def run(filename: String): (String, String) = {
    val messages = io.Source.fromFile(filename).getLines.toList
    val counts = messages.foldLeft(Map[Int, String]().withDefaultValue("")) {
      case (m, msg) =>
        msg.zipWithIndex.foldLeft(m) {
          case (m, (ch, idx)) =>
            m.updated(idx, m(idx) + ch)
        }
    }
    val str1 = counts
      .mapValues(_.groupBy(identity).mapValues(_.length).toList.sortBy(_._2).map(_._1).reverse.head)
      .toList.sorted.map(_._2).mkString
    val str2 = counts
      .mapValues(_.groupBy(identity).mapValues(_.length).toList.sortBy(_._2).map(_._1).head)
      .toList.sorted.map(_._2).mkString
    (str1, str2)
  }
}
