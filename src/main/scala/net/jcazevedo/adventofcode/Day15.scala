package net.jcazevedo.adventofcode

class Day15 extends DailyChallenge[Long, Long] {
  def run(filename: String): (Long, Long) = {
    def generator(firstValue: Long, factor: Long, valid: Long => Boolean = _ => true, remainder: Long = 2147483647l): Iterator[Long] = {
      def aux(value: Long) = (value * factor) % remainder
      Iterator.iterate(aux(firstValue))(aux).filter(valid)
    }

    def countPairs(generatorA: Iterator[Long], generatorB: Iterator[Long], nPairs: Int): Long =
      generatorA.zip(generatorB).take(nPairs).count {
        case (v1, v2) =>
          v1 % 65536 == v2 % 65536
      }

    val lines = io.Source.fromFile(filename).getLines.toList
    val gA = lines(0).stripPrefix("Generator A starts with ").toInt
    val gB = lines(1).stripPrefix("Generator B starts with ").toInt

    (
      countPairs(generator(gA, 16807), generator(gB, 48271), 40000000),
      countPairs(generator(gA, 16807, _ % 4 == 0), generator(gB, 48271, _ % 8 == 0), 5000000))
  }
}
