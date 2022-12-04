import scala.util.parsing.combinator.RegexParsers

object Day18 extends DailyChallenge[Long, Long] {
  sealed trait SnailfishNumber
  case class Regular(value: Int) extends SnailfishNumber {
    override def toString: String = value.toString
  }
  case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber {
    override def toString: String = s"[$left,$right]"
  }

  object SnailfishNumber {
    def parse(str: String): SnailfishNumber =
      SnailfishParser.parse(SnailfishParser.number, str).get

    def add(n1: SnailfishNumber, n2: SnailfishNumber): SnailfishNumber =
      reduce(Pair(n1, n2))

    def addToLeft(number: SnailfishNumber, toAdd: Int): SnailfishNumber =
      number match {
        case Regular(value)    => Regular(value + toAdd)
        case Pair(left, right) => Pair(addToLeft(left, toAdd), right)
      }

    def addToRight(number: SnailfishNumber, toAdd: Int): SnailfishNumber =
      number match {
        case Regular(value)    => Regular(value + toAdd)
        case Pair(left, right) => Pair(left, addToRight(right, toAdd))
      }

    def explode(number: SnailfishNumber, depth: Int = 0): (Int, SnailfishNumber, Int) =
      number match {
        case regular: Regular =>
          (0, regular, 0)

        case Pair(Regular(left), Regular(right)) if depth >= 4 =>
          (left, Regular(0), right)

        case pair @ Pair(Regular(_), Regular(_)) =>
          (0, pair, 0)

        case pair @ Pair(left, right) =>
          lazy val (leftAddL, newLeft, leftAddR) = explode(left, depth + 1)
          lazy val (rightAddL, newRight, rightAddR) = explode(right, depth + 1)

          if (newLeft != left) (leftAddL, Pair(newLeft, addToLeft(right, leftAddR)), 0)
          else if (newRight != right) (0, Pair(addToRight(left, rightAddL), newRight), rightAddR)
          else (0, pair, 0)
      }

    def split(number: SnailfishNumber): SnailfishNumber =
      number match {
        case Regular(value) if value >= 10 =>
          Pair(Regular(value / 2), Regular(value - value / 2))

        case regular: Regular =>
          regular

        case pair @ Pair(left, right) =>
          lazy val newLeft = split(left)
          lazy val newRight = split(right)

          if (newLeft != left) Pair(newLeft, right)
          else if (newRight != right) Pair(left, newRight)
          else pair
      }

    def reduce(number: SnailfishNumber): SnailfishNumber = {
      lazy val (_, tryExplode, _) = explode(number)
      lazy val trySplit = split(number)
      if (tryExplode != number) reduce(tryExplode)
      else if (trySplit != number) reduce(trySplit)
      else number
    }

    def magnitude(number: SnailfishNumber): Long =
      number match {
        case Regular(value)    => value
        case Pair(left, right) => 3L * magnitude(left) + 2L * magnitude(right)
      }
  }

  object SnailfishParser extends RegexParsers {
    def regular: Parser[Regular] = """(0|[1-9]\d*)""".r ^^ (v => Regular(v.toInt))
    def pair: Parser[Pair] = "[" ~> number ~ "," ~ number <~ "]" ^^ ({ case v1 ~ _ ~ v2 => Pair(v1, v2) })
    def number: Parser[SnailfishNumber] = pair | regular
  }

  def run(input: String): (Long, Long) = {
    val numbers = input.split("\n").map(SnailfishNumber.parse).toList

    val part1 =
      SnailfishNumber.magnitude(numbers.tail.foldLeft(numbers.head)((curr, next) => SnailfishNumber.add(curr, next)))
    val part2 = (for {
      l <- numbers
      r <- numbers
      if l != r
    } yield SnailfishNumber.magnitude(SnailfishNumber.add(l, r))).max

    (part1, part2)
  }
}
