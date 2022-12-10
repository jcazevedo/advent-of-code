object Day21 extends DailyChallenge[Long, Long] {
  def play(
      positions: Vector[Int],
      scores: Vector[Long],
      currentPlayer: Int,
      roll: () => Int,
      nRolls: Int = 0
  ): (Vector[Long], Int) = {
    val nextPos = (positions(currentPlayer) + roll() + roll() + roll() - 1) % 10 + 1
    val nextScore = scores(currentPlayer) + nextPos
    if (nextScore >= 1000) (scores.updated(currentPlayer, nextScore), nRolls + 3)
    else
      play(
        positions.updated(currentPlayer, nextPos),
        scores.updated(currentPlayer, nextScore),
        (currentPlayer + 1) % positions.length,
        roll,
        nRolls + 3
      )
  }

  def wins(start0: Int, start1: Int): (Long, Long) = {
    def diracRolls(maxV: Int, nRolls: Int): List[Int] =
      if (nRolls == 0) List.empty
      else if (nRolls == 1) (1 to maxV).toList
      else diracRolls(maxV, nRolls - 1).flatMap(v => (1 to maxV).map(v + _))

    val rolls = diracRolls(3, 3)

    val cache = Array.fill(11, 11, 31, 31, 2)((-1L, -1L))
    def aux(p0: Int, p1: Int, s0: Int, s1: Int, curr: Int): (Long, Long) = {
      if (cache(p0)(p1)(s0)(s1)(curr) == (-1L, -1L)) {
        var wins0 = 0L
        var wins1 = 0L

        if (curr == 0) {
          rolls.foreach { d =>
            val np0 = (p0 + d - 1) % 10 + 1
            val ns0 = s0 + np0
            if (ns0 >= 21) wins0 += 1
            else {
              val (w0, w1) = aux(np0, p1, ns0, s1, (curr + 1) % 2)
              wins0 += w0
              wins1 += w1
            }
          }
        } else {
          rolls.foreach { d =>
            val np1 = (p1 + d - 1) % 10 + 1
            val ns1 = s1 + np1
            if (ns1 >= 21) wins1 += 1
            else {
              val (w0, w1) = aux(p0, np1, s0, ns1, (curr + 1) % 2)
              wins0 += w0
              wins1 += w1
            }
          }
        }

        cache(p0)(p1)(s0)(s1)(curr) = (wins0, wins1)
      }
      cache(p0)(p1)(s0)(s1)(curr)
    }

    aux(start0, start1, 0, 0, 0)
  }

  def run(input: String): (Long, Long) = {
    val players = input
      .split("\n")
      .map(_.stripPrefix("Player "))
      .map(line => {
        val Array(_, pos) = line.split(" starting position: ")
        pos.toInt
      })
      .toVector

    val (scores1, rolls1) = play(
      players,
      Vector.fill(players.length)(0L),
      0, {
        var next = 1
        () => {
          val ans = next
          next = next % 100 + 1
          ans
        }
      }
    )

    val (score0, score1) = wins(players(0), players(1))

    val part1 = scores1.min * rolls1
    val part2 = math.max(score0, score1)

    (part1, part2)
  }
}
