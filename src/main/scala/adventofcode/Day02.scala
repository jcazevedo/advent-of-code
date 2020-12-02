package adventofcode

class Day02 extends DailyChallenge[Int, Int] {
  case class PasswordPolicy(lo: Int, hi: Int, ch: Char)

  def isValidPart1(policy: PasswordPolicy, password: String): Boolean = {
    val chCnt = password.count(_ == policy.ch)
    chCnt >= policy.lo && chCnt <= policy.hi
  }

  def isValidPart2(policy: PasswordPolicy, password: String): Boolean = {
    val chSet = Set(password(policy.lo - 1), password(policy.hi - 1))
    chSet.size == 2 && chSet(policy.ch)
  }

  def part1(restrictions: List[(PasswordPolicy, String)]): Int =
    restrictions.count((isValidPart1 _).tupled)

  def part2(restrictions: List[(PasswordPolicy, String)]): Int =
    restrictions.count((isValidPart2 _).tupled)

  def run(input: String): (Int, Int) = {
    val passwordRegex = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
    val restrictions = input
      .split("\n")
      .map { case passwordRegex(lo, hi, ch, password) =>
        (PasswordPolicy(lo.toInt, hi.toInt, ch.head), password)
      }
      .toList
    (part1(restrictions), part2(restrictions))
  }
}
