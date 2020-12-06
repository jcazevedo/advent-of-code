package adventofcode

class Day06 extends DailyChallenge[Int, Int] {
  def part1(answers: List[List[String]]): Int =
    answers.map(_.flatMap(_.toSet).toSet.size).sum

  def part2(answers: List[List[String]]): Int =
    answers.map(_.map(_.toSet).reduce(_ & _).size).sum

  def run(input: String): (Int, Int) = {
    val answers = input.split("\n").foldRight(List(List.empty[String])) { case (curr, acc) =>
      curr match {
        case ""    => List.empty :: acc
        case other => (other :: acc.head) :: acc.tail
      }
    }
    (part1(answers), part2(answers))
  }
}
