object Day06 extends DailyChallenge[Int, Int] {
  def startOfMessageMarker(signal: String, nDistinct: Int): Int =
    signal.zipWithIndex.sliding(nDistinct).filter(_.map(_._1).toSet.size == nDistinct).next.last._2 + 1

  def run(input: String): (Int, Int) = {
    val lines = input.split("\n").toList
    val signal = lines.head

    val part1 = startOfMessageMarker(signal, 4)
    val part2 = startOfMessageMarker(signal, 14)

    (part1, part2)
  }
}
