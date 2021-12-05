package net.jcazevedo.adventofcode

class Day04 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines.toList

    def valid(line: String) = {
      val words = line.split("\\s+").toList
      words.length == words.toSet.size
    }

    def anagram(word1: String, word2: String): Boolean =
      word1.sorted == word2.sorted

    def valid1(line: String) = {
      def aux(curr: String, other: List[String]): Boolean =
        if (other.isEmpty)
          true
        else
          other.forall(!anagram(_, curr)) && aux(other.head, other.tail)
      val words = line.split("\\s+").toList
      aux(words.head, words.tail)
    }

    val res1 = lines.count(valid)
    val res2 = lines.count(valid1)
    (res1, res2)
  }
}
