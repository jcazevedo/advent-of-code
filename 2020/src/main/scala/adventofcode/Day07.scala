package adventofcode

class Day07 extends DailyChallenge[Int, Int] {
  case class Bag(color: String, contents: Map[String, Int])

  def part1(bags: List[Bag]): Int = {
    val contents = bags.map(bag => bag.color -> bag.contents.keySet).toMap

    def canHold(color: String, target: String): Boolean =
      contents.get(color).exists(_.exists(content => content == target || canHold(content, target)))

    contents.keySet.count(canHold(_, "shiny gold"))
  }

  def part2(bags: List[Bag]): Int = {
    val contents = bags.map(bag => bag.color -> bag).toMap

    def totalBags(curr: String): Int = {
      contents.get(curr).fold(0) { bag =>
        bag.contents.map { case (color, quantity) =>
          quantity + quantity * totalBags(color)
        }.sum
      }
    }

    totalBags("shiny gold")
  }

  def run(input: String): (Int, Int) = {
    val contentRegex = raw"(\d+) ([^\.]+)? bags?\.?".r
    val bags = input
      .split("\n")
      .map { str =>
        val Array(mainColor, rest) = str.split(" contain ")
        val contents = rest.split(", ")
        Bag(
          mainColor.dropRight(5),
          if (rest == "no other bags.") Map.empty
          else
            contents.map { case contentRegex(quantity, color) =>
              color -> quantity.toInt
            }.toMap
        )
      }
      .toList

    (part1(bags), part2(bags))
  }
}
