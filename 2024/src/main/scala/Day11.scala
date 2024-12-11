import scala.collection.mutable

object Day11 extends DailyChallenge[Long, Long] {
  case class Stone(value: BigInt)

  def run(input: String): (Long, Long) = {
    val stones = input.split("\\s+").map(s => Stone(BigInt(s))).toVector

    val cache = mutable.Map.empty[(Stone, Int), Long]
    def count(stone: Stone, blinks: Int): Long =
      if (blinks == 0) 1L
      else if (cache.contains((stone, blinks))) cache((stone, blinks))
      else {
        val cnt = stone match {
          case Stone(value) if value == BigInt("0") =>
            count(Stone(BigInt("1")), blinks - 1)
          case Stone(bigint) if bigint.toString.length % 2 == 0 =>
            val str = bigint.toString
            val len = bigint.toString.length
            count(Stone(BigInt(str.take(len / 2))), blinks - 1) +
              count(Stone(BigInt(str.drop(len / 2))), blinks - 1)
          case Stone(bigint) =>
            count(Stone(bigint * 2024), blinks - 1)
        }
        cache((stone, blinks)) = cnt
        cnt
      }

    val part1 = stones.map(count(_, 25)).sum
    val part2 = stones.map(count(_, 75)).sum

    (part1, part2)
  }
}
