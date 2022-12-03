object Day03 extends DailyChallenge[Long, Long] {
  def binaryStringToLong(str: String): Long =
    str.foldLeft(0L)((curr, dig) => curr * 2 + (dig - '0'))

  def bitAt(num: Long, digitLength: Int, digit: Int): Int =
    if ((num & (1 << (digitLength - 1 - digit))) > 0) 1 else 0

  def gammaRate(report: List[Long], digitLength: Int): Long = {
    val cntZero = Array.fill(digitLength)(0)
    val cntOne = Array.fill(digitLength)(0)

    report.foreach(num =>
      (0 until digitLength).foreach(i => if (bitAt(num, digitLength, i) == 1) cntOne(i) += 1 else cntZero(i) += 1)
    )

    (0 until digitLength).foldLeft(0L)((curr, dig) => curr * 2 + (if (cntOne(dig) > cntZero(dig)) 1 else 0))
  }

  def epsilonRate(report: List[Long], digitLength: Int): Long = {
    val cntZero = Array.fill(digitLength)(0)
    val cntOne = Array.fill(digitLength)(0)

    report.foreach(num =>
      (0 until digitLength).foreach(i => if (bitAt(num, digitLength, i) == 1) cntOne(i) += 1 else cntZero(i) += 1)
    )

    (0 until digitLength).foldLeft(0L)((curr, dig) => curr * 2 + (if (cntOne(dig) < cntZero(dig)) 1 else 0))
  }

  def oxygenGeneratorRating(report: List[Long], digitLength: Int, digit: Int = 0): Long =
    report match {
      case Nil         => -1L // This shouldn't happen.
      case head :: Nil => head
      case _ =>
        var cntZero = 0
        var cntOne = 0
        report.foreach(num => if (bitAt(num, digitLength, digit) == 1) cntOne += 1 else cntZero += 1)
        oxygenGeneratorRating(
          if (cntOne >= cntZero) report.filter(bitAt(_, digitLength, digit) == 1)
          else report.filter(bitAt(_, digitLength, digit) == 0),
          digitLength,
          digit + 1
        )
    }

  def co2ScrubberRating(report: List[Long], digitLength: Int, digit: Int = 0): Long =
    report match {
      case Nil         => -1L // This shouldn't happen.
      case head :: Nil => head
      case _ =>
        var cntZero = 0
        var cntOne = 0
        report.foreach(num => if (bitAt(num, digitLength, digit) == 1) cntOne += 1 else cntZero += 1)
        co2ScrubberRating(
          if (cntZero <= cntOne) report.filter(bitAt(_, digitLength, digit) == 0)
          else report.filter(bitAt(_, digitLength, digit) == 1),
          digitLength,
          digit + 1
        )
    }

  def run(input: String): (Long, Long) = {
    val lines = input.split("\n")
    val digitLength = lines.head.length
    val report = lines.map(binaryStringToLong).toList

    val part1 = gammaRate(report, digitLength) * epsilonRate(report, digitLength)
    val part2 = oxygenGeneratorRating(report, digitLength) * co2ScrubberRating(report, digitLength)

    (part1, part2)
  }
}
