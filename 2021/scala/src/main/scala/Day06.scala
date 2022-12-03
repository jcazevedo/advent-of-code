import scala.collection.mutable

object Day06 extends DailyChallenge[Long, Long] {
  val countFishesCache = mutable.Map.empty[(Int, Int), Long]

  def countFishes(timer: Int, days: Int): Long = {
    if (!countFishesCache.contains((timer, days))) {
      countFishesCache((timer, days)) =
        if (days <= 0) 1
        else if (timer == 0) countFishes(6, days - 1) + countFishes(8, days - 1)
        else countFishes(timer - 1, days - 1)
    }
    countFishesCache((timer, days))
  }

  def fishesAfterDays(fishes: List[Int], days: Int): Long =
    fishes.map(countFishes(_, days)).sum

  def run(input: String): (Long, Long) = {
    val fishes = input.trim.split(",").map(_.toInt).toList

    val part1 = fishesAfterDays(fishes, 80)
    val part2 = fishesAfterDays(fishes, 256)

    (part1, part2)
  }
}
