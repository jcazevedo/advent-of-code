package adventofcode

class Day13 extends DailyChallenge[Long, Long] {
  def part1(time: Long, buses: List[Long]): Long = {
    val times = buses.filter(_ >= 0).map(id => id -> ((id - time % id) % id))
    val (id, wait) = times.minBy(_._2)
    id * wait
  }

  def part2(buses: List[Long]): Long = {
    def modInv(a: Long, b: Long): Long = {
      def go(a: Long, b: Long, x0: Long, x1: Long): Long =
        if (a > 1) go(b, a % b, x1 - (a / b) * x0, x0) else x1
      if (b == 1) 1
      else {
        val x1 = go(a, b, 0, 1)
        if (x1 < 0) x1 + b else x1
      }
    }

    val prod = buses.filter(_ > 0).product
    buses.zipWithIndex.map {
      case (-1, _) => 0L
      case (n, i) =>
        val m = prod / n
        m * (n - i) * modInv(m, n)
    }.sum % prod
  }

  def run(input: String): (Long, Long) = {
    val Array(timeS, busesS) = input.split("\n")
    val time = timeS.toLong
    val buses = busesS.split(",").map(x => if (x == "x") -1 else x.toLong).toList
    (part1(time, buses), part2(buses))
  }
}
