import scala.collection.mutable

object Day10 extends DailyChallenge[Long, Long] {
  final val IllegalScores: Map[Char, Int] = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  final val CompletionScores: Map[Char, Int] = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  final val PairsToOpen: Map[Char, Char] = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  final val PairsToClose: Map[Char, Char] = PairsToOpen.map({ case (k, v) => v -> k }).toMap

  def firstIllegalCharacter(line: String): Option[Char] = {
    var res: Option[Char] = None

    val s = mutable.Stack.empty[Char]
    line.indices.foreach(idx =>
      if (res.isEmpty) {
        val ch = line(idx)
        if (PairsToOpen.contains(ch)) s.push(ch)
        else if (s.isEmpty || s.head != PairsToClose(ch)) res = Some(ch)
        else s.pop()
      }
    )

    res
  }

  def scoreToComplete(line: String): Long = {
    var res: Option[Char] = None

    val s = mutable.Stack.empty[Char]
    line.indices.foreach(idx =>
      if (res.isEmpty) {
        val ch = line(idx)
        if (PairsToOpen.contains(ch)) s.push(ch)
        else if (s.isEmpty || s.head != PairsToClose(ch)) res = Some(ch)
        else s.pop()
      }
    )

    if (res.nonEmpty) 0L
    else {
      var score = 0L
      while (s.nonEmpty) {
        score = score * 5L + CompletionScores(PairsToOpen(s.head))
        s.pop()
      }
      score
    }
  }

  def run(input: String): (Long, Long) = {
    val lines = input.split("\n").toList

    val part1 = lines.flatMap(firstIllegalCharacter).map(IllegalScores).sum
    val part2 = {
      val scores = lines.map(scoreToComplete).filter(_ != 0L).sorted
      scores(scores.length / 2)
    }

    (part1, part2)
  }
}
