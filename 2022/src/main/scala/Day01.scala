object Day01 extends DailyChallenge[Int, Int] {
  def run(input: String): (Int, Int) = {
    val caloriesCarried = input.split("\n").foldRight(List.empty[Int]) {
      case (str, Nil)                      => List(str.toInt)
      case (str, curr) if str.trim.isEmpty => 0 :: curr
      case (str, h :: t)                   => (h + str.toInt) :: t
    }

    val part1 = caloriesCarried.max
    val part2 = caloriesCarried.sorted.takeRight(3).sum

    (part1, part2)
  }
}
