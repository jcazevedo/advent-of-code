object Day25 extends DailyChallenge[String, String] {
  def toDecimal(snafu: String): Long = {
    var ans = 0L
    var pow = 1L

    snafu.reverse
      .map {
        case ch if ch.isDigit => ch - '0'
        case '-'              => -1
        case '='              => -2
      }
      .foreach { x =>
        ans += x * pow
        pow *= 5L
      }

    ans
  }

  def toSnafu(dec: Long): String = {
    var curr = dec
    val ans = new StringBuilder()

    while (curr > 0) {
      val mod = curr % 5
      mod match {
        case 0 => ans += '0'
        case 1 => ans += '1'
        case 2 => ans += '2'
        case 3 => ans += '='
        case 4 => ans += '-'
      }
      curr = curr / 5 + (if (mod > 2) 1 else 0)
    }

    ans.toString.reverse
  }

  def run(input: String): (String, String) = {
    val numbers = input.split("\n").map(_.trim)

    val part1 = toSnafu(numbers.map(toDecimal).sum)

    (part1, "")
  }
}
