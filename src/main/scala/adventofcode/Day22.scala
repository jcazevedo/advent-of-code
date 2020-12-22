package adventofcode

class Day22 extends DailyChallenge[Int, Int] {
  case class Deck(cards: Vector[Int]) {
    def isEmpty = cards.isEmpty
  }
  case class State(player1: Deck, player2: Deck, winner: Option[Int])

  def play(state: State): State = {
    if (state.player1.isEmpty) state.copy(winner = Some(2))
    else if (state.player2.isEmpty) state.copy(winner = Some(1))
    else {
      val h1 +: t1 = state.player1.cards
      val h2 +: t2 = state.player2.cards
      if (h1 > h2) {
        play(
          state.copy(
            player1 = state.player1.copy(cards = t1 ++ Vector(h1, h2)),
            player2 = state.player2.copy(cards = t2)
          )
        )
      } else {
        play(
          state.copy(
            player1 = state.player1.copy(cards = t1),
            player2 = state.player2.copy(cards = t2 ++ Vector(h2, h1))
          )
        )
      }
    }
  }

  def playRecursive(state: State, visited: Set[(Deck, Deck)] = Set.empty): State = {
    if (visited((state.player1, state.player2))) state.copy(winner = Some(1))
    else if (state.player1.isEmpty) state.copy(winner = Some(2))
    else if (state.player2.isEmpty) state.copy(winner = Some(1))
    else {
      val nextVisited = visited + ((state.player1, state.player2))
      val h1 +: t1 = state.player1.cards
      val h2 +: t2 = state.player2.cards
      val winner = if (t1.length >= h1 && t2.length >= h2) {
        val finalSt = playRecursive(State(player1 = Deck(t1.take(h1)), player2 = Deck(t2.take(h2)), None), nextVisited)
        assert(finalSt.winner.nonEmpty)
        finalSt.winner.get
      } else if (h1 > h2) {
        1
      } else {
        2
      }
      if (winner == 1)
        playRecursive(
          state
            .copy(player1 = state.player1.copy(cards = t1 ++ Vector(h1, h2)), player2 = state.player2.copy(cards = t2)),
          nextVisited
        )
      else
        playRecursive(
          state
            .copy(player1 = state.player1.copy(cards = t1), player2 = state.player2.copy(cards = t2 ++ Vector(h2, h1))),
          nextVisited
        )
    }
  }

  def playWith(deck1: Deck, deck2: Deck, f: State => State): Int = {
    val initialSt = State(deck1, deck2, None)
    val finalSt = f(initialSt)
    assert(finalSt.winner.nonEmpty)
    val finalDeck = if (finalSt.winner.contains(1)) finalSt.player1.cards else finalSt.player2.cards
    finalDeck.reverse.zipWithIndex.map { case (v, i) => v * (i + 1) }.sum
  }

  def part1(deck1: Deck, deck2: Deck): Int =
    playWith(deck1, deck2, play)

  def part2(deck1: Deck, deck2: Deck): Int =
    playWith(deck1, deck2, playRecursive(_))

  def mkDeck(input: String): Deck =
    Deck(input.split("\n").drop(1).map(_.toInt).toVector)

  def run(input: String): (Int, Int) = {
    val Array(deck1, deck2) = input.split("\n\n").map(mkDeck)
    (part1(deck1, deck2), part2(deck1, deck2))
  }
}
