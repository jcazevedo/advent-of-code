package adventofcode

class Day25 extends DailyChallenge[Long, Long] {
  final val MOD = 20201227

  def transform(value: Long, loopSize: Int): Long = {
    if (loopSize == 0) 1
    else if (loopSize == 1) value % MOD
    else if (loopSize % 2 == 0) transform((value * value) % MOD, loopSize / 2)
    else (value * transform((value * value) % MOD, (loopSize - 1) / 2)) % MOD
  }

  def loopSize(pk: Long): Int = {
    def go(curr: Int, v: Long = 1): Int =
      if (v == pk) curr
      else go(curr + 1, (v * 7L) % MOD)

    go(0)
  }

  def part1(cardPK: Long, doorPK: Long): Long = {
    val cardLS = loopSize(cardPK)
    val doorLS = loopSize(doorPK)
    val enc1 = transform(cardPK, doorLS)
    val enc2 = transform(doorPK, cardLS)
    assert(enc1 == enc2)
    enc1
  }

  def run(input: String): (Long, Long) = {
    val Array(cardPK, doorPK) = input.trim.split("\n").map(_.toLong)
    (part1(cardPK, doorPK), -1)
  }
}
