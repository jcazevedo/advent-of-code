package net.jcazevedo.adventofcode

class Day15 extends DailyChallenge[Int, Int] {
  case class Disc(id: Int, positions: Int, currentPosition: Int) {
    def tick: Disc = this.copy(currentPosition = (currentPosition + 1) % positions)
  }

  def toDisc(discDefinition: String): Disc = {
    val ss = discDefinition.split("\\s+")
    Disc(ss(1).drop(1).toInt, ss(3).toInt, ss(11).dropRight(1).toInt)
  }

  def discRotation(start: List[Disc]): Stream[List[Disc]] =
    start #:: discRotation(start.map(_.tick))

  def discStream(start: List[Disc]): Stream[(Int, List[Disc])] =
    Stream.from(0).zip(discRotation(start))

  def valid(discs: List[Disc]): Boolean = {
    discs.foldLeft((1, true)) {
      case ((i, valid), disc) =>
        val finalDisc = (0 until i).foldLeft(disc) { case (d, _) => d.tick }
        (i + 1, valid & finalDisc.currentPosition == 0)
    }._2
  }

  def run(filename: String): (Int, Int) = {
    val discDefinitions = io.Source.fromFile(filename).getLines.toList
    val discs = discDefinitions.map(toDisc)
    val s1 = discStream(discs)
    val s2 = discStream(discs :+ Disc(discs.length + 1, 11, 0))
    (s1.dropWhile(v => !valid(v._2)).head._1, s2.dropWhile(v => !valid(v._2)).head._1)
  }
}
