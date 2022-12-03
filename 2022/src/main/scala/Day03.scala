object Day03 extends DailyChallenge[Int, Int] {
  def priority(item: Char): Int =
    if (item <= 'Z') (item - 'A') + 27
    else (item - 'a') + 1

  def repeatedItem(rucksack: String): Char = {
    val (first, second) = rucksack.splitAt(rucksack.length / 2)
    (first.toSet & second.toSet).head
  }

  def badge(rucksacks: List[String]): Char =
    rucksacks.map(_.toSet).reduce(_ & _).head

  def run(input: String): (Int, Int) = {
    val rucksacks = input.split("\n").toList

    val part1 = rucksacks.map(repeatedItem).map(priority).sum
    val part2 = rucksacks.sliding(3, 3).map(badge).map(priority).sum

    (part1, part2)
  }
}
