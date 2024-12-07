object Day07 extends DailyChallenge[Long, Long] {
  case class Equation(testValue: Long, numbers: List[Long])

  def isTrue(equation: Equation, operations: List[(Long, Long) => Long]): Boolean = {
    def isTrueAux(target: Long, curr: Long, remaining: List[Long]): Boolean =
      remaining match {
        case Nil    => target == curr
        case h :: t => operations.exists(op => isTrueAux(target, op(curr, h), t))
      }

    isTrueAux(equation.testValue, equation.numbers.head, equation.numbers.tail)
  }

  def run(input: String): (Long, Long) = {
    val equations = input
      .split("\n")
      .map({ case s"$testValue: $numbers" =>
        Equation(testValue.toLong, numbers.split("\\s+").map(_.toLong).toList)
      })

    val part1 = equations
      .filter(isTrue(_, operations = List(_ + _, _ * _)))
      .map(_.testValue)
      .sum
    val part2 = equations
      .filter(isTrue(_, operations = List(_ + _, _ * _, (v1, v2) => s"$v1$v2".toLong)))
      .map(_.testValue)
      .sum

    (part1, part2)
  }
}
