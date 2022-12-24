object Day20 extends DailyChallenge[Long, Long] {
  def nth(file: Vector[Long], i: Int): Long =
    file((i + file.indexOf(0)) % file.length)

  def mix(file: Vector[Long], decryptionKey: Long, runs: Int): Vector[Long] = {
    val indexed = file.map(_ * decryptionKey).zipWithIndex

    (0 until runs)
      .foldLeft(indexed)((current, _) =>
        indexed
          .foldLeft(current)((curr, num) => {
            val current = curr.indexOf(num)
            val next = {
              val possibleNeg = ((current + num._1) % (curr.length - 1)).toInt
              if (possibleNeg < 0) curr.length - 1 + possibleNeg
              else possibleNeg
            }
            val withoutCurrent = curr.take(current) ++ curr.drop(current + 1)
            val withNext = withoutCurrent.take(next) ++ Vector(num) ++ withoutCurrent.drop(next)
            withNext
          })
      )
      .map(_._1)
  }

  def run(input: String): (Long, Long) = {
    val file = input.split("\n").map(_.toLong).toVector

    val part1 = {
      val afterMix = mix(file, decryptionKey = 1L, runs = 1)
      nth(afterMix, 1000) + nth(afterMix, 2000) + nth(afterMix, 3000)
    }

    val part2 = {
      val afterMix = mix(file, decryptionKey = 811589153L, runs = 10)
      nth(afterMix, 1000) + nth(afterMix, 2000) + nth(afterMix, 3000)
    }

    (part1, part2)
  }
}
