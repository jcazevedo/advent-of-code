object Day08 extends DailyChallenge[Int, Int] {
  final val digitToWires: Map[Int, String] =
    Map(
      0 -> "abcefg",
      1 -> "cf",
      2 -> "acdeg",
      3 -> "acdfg",
      4 -> "bcdf",
      5 -> "abdfg",
      6 -> "abdefg",
      7 -> "acf",
      8 -> "abcdefg",
      9 -> "abcdfg"
    )

  final val wiresToDigits: Map[String, Int] = digitToWires.map({ case (k, v) => v -> k })

  case class Line(patterns: Vector[String], outputs: Vector[String])

  final val wireAssignments: Set[Map[Char, Char]] = {
    def aux(curr: Char, r: Set[Char]): Set[Map[Char, Char]] =
      if (r.size == 0) Set(Map.empty)
      else for { y <- r; rest <- aux((curr + 1).toChar, r - y) } yield rest + (curr -> y)

    aux('a', "abcdefg".toSet)
  }

  def patternAssignments(patterns: Vector[String]): Set[Map[String, Int]] = {
    def aux(rem: Vector[String], digits: Set[Int]): Set[Map[String, Int]] =
      if (digits.isEmpty) Set(Map.empty)
      else
        for {
          d <- digits.filter(digitToWires(_).length == rem.head.length)
          rest <- aux(rem.tail, digits - d)
        } yield rest + (rem.head -> d)

    aux(patterns, digitToWires.keySet)
  }

  def decode(patterns: Vector[String]): Map[Char, Char] = {
    val assignments = patternAssignments(patterns)

    wireAssignments
      .find(assignment =>
        assignments.exists(_.forall({ case (pattern, digit) => pattern.map(assignment).sorted == digitToWires(digit) }))
      )
      .get
  }

  def decodeOutputStr(line: Line): String = {
    val assignment = decode(line.patterns)
    line.outputs.map(digit => wiresToDigits(digit.map(assignment).sorted).toString).mkString
  }

  def decodeOutputInt(line: Line): Int = {
    val assignment = decode(line.patterns)
    line.outputs.map(digit => wiresToDigits(digit.map(assignment).sorted)).foldLeft(0)((curr, next) => curr * 10 + next)
  }

  def run(input: String): (Int, Int) = {
    val lines = input
      .split("\n")
      .map(line => {
        val Array(patterns, outputs) = line.split(" \\| ")
        Line(patterns.split(" ").toVector, outputs.split(" ").toVector)
      })
      .toList

    val part1 = lines.map(decodeOutputStr).map(str => str.count(d => d == '1' || d == '4' || d == '7' || d == '8')).sum
    val part2 = lines.map(decodeOutputInt).sum

    (part1, part2)
  }
}
