import scala.collection.mutable

object Day22 extends DailyChallenge[Long, Long] {
  def mix(a: Long, b: Long): Long = a ^ b
  def prune(a: Long): Long = a % 16777216

  def nextSecretNumber(number: Long): Long = {
    val a = prune(mix(number * 64, number))
    val b = prune(mix(a / 32, a))
    val c = prune(mix(b * 2048, b))
    c
  }

  def rng1(start: Long): Iterator[Long] =
    Iterator.iterate(start)(nextSecretNumber)

  def rng2(start: Long): Iterator[Long] =
    rng1(start).map(_ % 10)

  def possibleSequences(length: Int): List[List[Long]] =
    if (length == 0) List(List.empty)
    else possibleSequences(length - 1).flatMap(rest => (-18L to 18L).map(_ :: rest))

  def run(input: String): (Long, Long) = {
    val secretNumbers = input.split("\n").map(_.toLong).toVector

    val part1 = secretNumbers.map(number => rng1(number).drop(2000).next).sum
    val part2 = {
      val possible = possibleSequences(4)
      val sequences = secretNumbers.map(number => rng2(number).take(2001).toList).toVector
      val changesAndPrice = sequences.map(
        _.sliding(5).toList
          .map(window => {
            val diffs = window.sliding(2).map(l => l(1) - l(0)).toList
            val next = window.last
            (diffs, next)
          })
          .groupMapReduce(_._1)(_._2)((a, b) => a)
      )
      val maxBySequence = mutable.Map.empty[List[Long], Long]
      changesAndPrice.foreach(m =>
        m.foreach((k, v) =>
          maxBySequence.updateWith(k)({
            case None       => Some(v)
            case Some(curr) => Some(curr + v)
          })
        )
      )
      maxBySequence.maxBy(_._2)._2
    }

    (part1, part2)
  }
}
