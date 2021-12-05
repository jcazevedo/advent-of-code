package net.jcazevedo.adventofcode

class Day05 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def vowel(c: Char) = "aeiou".indexOf(c) >= 0

    def nice(s: String): Boolean = {
      val nVowels = s.filter(vowel).size
      val twices = ('a' to 'z').map(_.toString * 2).map(s.indexOf).count(_ >= 0)
      val forbidden = Seq("ab", "cd", "pq", "xy").map(s.indexOf).count(_ >= 0)

      nVowels >= 3 && twices > 0 && forbidden == 0
    }

    def nice2(s: String): Boolean = {
      val twoLetterPairs = s.sliding(2).zipWithIndex.exists {
        case (w, i) =>
          s.substring(i + 2).indexOf(w) >= 0
      }
      val singleRepeats = s.substring(0, s.size - 2).zipWithIndex.exists {
        case (c, i) =>
          s(i + 2) == c
      }
      twoLetterPairs && singleRepeats
    }

    val strings = io.Source.fromFile(filename).getLines.toList
    (strings.count(nice), strings.count(nice2))
  }
}
