import scala.collection.mutable

object Day11 extends DailyChallenge[Int, Int] {
  case class State(grid: Vector[Vector[Int]]) {
    def next: (Int, State) = {
      val q = mutable.Queue.empty[(Int, Int)]
      val mut = grid.map(_.toArray).toArray
      var flashes = 0

      (0 until grid.length).foreach(r => (0 until grid(r).length).foreach(c => q.enqueue((r, c))))

      while (q.nonEmpty) {
        val (nr, nc) = q.dequeue()
        mut(nr)(nc) += 1
        if (mut(nr)(nc) == 10) {
          flashes += 1
          for {
            dr <- -1 to 1
            dc <- -1 to 1
            if (nr + dr, nc + dc) != (nr, nc)
            if nr + dr >= 0 && nr + dr < grid.length
            if nc + dc >= 0 && nc + dc < grid(nr).length
          } q.enqueue((nr + dr, nc + dc))
        }
      }

      (flashes, State(mut.map(_.map(x => if (x > 9) 0 else x).toVector).toVector))
    }
  }

  def run(input: String): (Int, Int) = {
    val initialState = State(input.split("\n").map(_.map(_ - '0').toVector).toVector)

    val part1 = (1 to 100)
      .foldLeft((0, initialState))({ case ((flashes, state), _) =>
        val (nFlashes, nState) = state.next
        (flashes + nFlashes, nState)
      })
      ._1
    val part2 = LazyList
      .iterate((0, initialState))({ case (_, state) => state.next })
      .zipWithIndex
      .dropWhile({ case ((flashes, state), _) => flashes != (state.grid.length * state.grid(0).length) })
      .head
      ._2

    (part1, part2)
  }
}
