object Day01 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val caloriesCarried = input.split("\n").map(_.trim).foldRight(List.empty[Int]) {
      case (str, Nil)    => 0 :: Nil
      case ("", curr)    => 0 :: curr
      case (str, h :: t) => (h + str.toInt) :: t
    }

    val part1 = caloriesCarried.max
    val part2 = caloriesCarried.sorted.takeRight(3).sum

    (part1, part2)
  }
}
