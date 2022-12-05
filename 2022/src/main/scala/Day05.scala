object Day05 extends DailyChallenge[String, String] {
  case class State(crates: Map[Int, List[Char]]) {
    def move9000(mv: Move): State = {
      val Move(n, from, to) = mv
      (0 until n).foldLeft(this)((state, _) =>
        val currTo = state.crates.getOrElse(to, List.empty)
        val currFrom = state.crates.getOrElse(from, List.empty)
        state.copy(crates = state.crates.updated(to, currFrom.head :: currTo).updated(from, currFrom.tail))
      )
    }

    def move9001(mv: Move): State = {
      val Move(n, from, to) = mv
      val currTo = crates.getOrElse(to, List.empty)
      val currFrom = crates.getOrElse(from, List.empty)
      copy(crates = crates.updated(to, currFrom.take(n) ++ currTo).updated(from, currFrom.drop(n)))
    }
  }

  case class Move(n: Int, from: Int, to: Int)

  def buildCrates(input: Vector[String]): State = {
    val idxStr = input.last

    idxStr.indices.foldLeft(State(Map.empty))((state, idx) =>
      if (idxStr(idx) == ' ') state
      else {
        val stack = idxStr(idx) - '0'
        input.init.reverse.foldLeft(state)((nextState, line) =>
          if (idx < line.length && line(idx).isLetter)
            nextState.copy(crates =
              nextState.crates.updated(stack, line(idx) :: nextState.crates.getOrElse(stack, List.empty))
            )
          else nextState
        )
      }
    )
  }

  def run(input: String): (String, String) = {
    val lines = input.split("\n").toList

    val initialState = buildCrates(lines.takeWhile(_.nonEmpty).toVector)
    val instructions = lines
      .dropWhile(_.nonEmpty)
      .drop(1)
      .map(line => {
        val Array(n, rest) = line.stripPrefix("move ").split(" from ")
        val Array(a, b) = rest.split(" to ")
        Move(n.toInt, a.toInt, b.toInt)
      })

    val finalState9000 = instructions.foldLeft(initialState)(_.move9000(_))
    val finalState9001 = instructions.foldLeft(initialState)(_.move9001(_))

    val part1 = finalState9000.crates.toList.sortBy(_._1).map(_._2.head).mkString
    val part2 = finalState9001.crates.toList.sortBy(_._1).map(_._2.head).mkString

    (part1, part2)
  }
}
