package adventofcode

class Day19 extends DailyChallenge[Int, Int] {
  sealed trait Match
  case class ExactMatch(str: String) extends Match
  case class RuleMatch(id: Int) extends Match

  case class Rule(matches: List[List[Match]]) {
    private def unmatched(str: String, rules: Map[Int, Rule]): Set[Int] = {
      def matchesRules(curr: String, toMatch: List[Match]): Set[Int] =
        toMatch match {
          case ExactMatch(prefix) :: t =>
            if (curr.startsWith(prefix)) matchesRules(curr.stripPrefix(prefix), t) else Set.empty
          case RuleMatch(id) :: t =>
            for {
              p1 <- rules(id).unmatched(curr, rules)
              p2 <- matchesRules(curr.takeRight(p1), t)
            } yield p2
          case Nil =>
            Set(curr.length)
        }

      matches.flatMap(matchesRules(str, _)).toSet
    }

    def matches(str: String, rules: Map[Int, Rule]) =
      unmatched(str, rules)(0)
  }

  def run(input: String): (Int, Int) = {
    val msgRegex = raw"""(\d+): (.*)""".r

    val (rules, messages) = input.split("\n").foldRight((Map.empty[Int, Rule], List.empty[String])) {
      case ("", (currRules, currMsgs)) => (currRules, currMsgs)
      case (str, (currRules, currMsgs)) =>
        str match {
          case msgRegex(id, rest) =>
            val opts = rest.split(" \\| ").toList.map { part =>
              part
                .split(" ")
                .map { str =>
                  if (str.startsWith("\"")) ExactMatch(str.drop(1).dropRight(1))
                  else RuleMatch(str.toInt)
                }
                .toList
            }
            (currRules.updated(id.toInt, Rule(opts)), currMsgs)
          case msg => (currRules, msg :: currMsgs)
        }
    }

    val newRules = rules
      .updated(8, Rule(List(List(RuleMatch(42)), List(RuleMatch(42), RuleMatch(8)))))
      .updated(11, Rule(List(List(RuleMatch(42), RuleMatch(31)), List(RuleMatch(42), RuleMatch(11), RuleMatch(31)))))

    (messages.count(rules(0).matches(_, rules)), messages.count(newRules(0).matches(_, newRules)))
  }
}
