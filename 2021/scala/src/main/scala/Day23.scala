import scala.collection.mutable

object Day23 extends DailyChallenge[Int, Int] {
  final val EnergyRequired: Map[Char, Int] =
    Map(
      'A' -> 1,
      'B' -> 10,
      'C' -> 100,
      'D' -> 1000
    )

  final val TargetColumn: Map[Char, Int] =
    Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)

  final val Directions: List[(Int, Int)] =
    List((0, 1), (1, 0), (-1, 0), (0, -1))

  def energyRequired(
      grid: Vector[String],
      from: (Int, Int),
      to: (Int, Int),
      unitCost: Int,
      amphipods: Map[Char, Set[(Int, Int)]]
  ): Option[Int] = {
    val q = mutable.Queue.empty[(Int, (Int, Int))]
    val visited = mutable.Set.empty[(Int, Int)]

    amphipods.values.toSet.flatten.foreach(visited.addOne)
    visited.add(from)
    q.enqueue((0, from))
    var ans: Option[Int] = None

    while (q.nonEmpty && ans.isEmpty) {
      val (currEnergy, (currI, currJ)) = q.dequeue()
      Directions.foreach { case (di, dj) =>
        val ni = currI + di
        val nj = currJ + dj
        if (
          ni >= 0 && ni < grid.length && nj >= 0 && nj < grid(ni).length && grid(ni)(nj) == '.' && !visited((ni, nj))
        ) {
          val nextEnergy = currEnergy + unitCost
          if ((ni, nj) == to)
            ans = Some(nextEnergy)
          else {
            visited.add((ni, nj))
            q.enqueue((nextEnergy, (ni, nj)))
          }
        }
      }
    }

    ans
  }

  def done(positions: Map[Char, Set[(Int, Int)]]): Boolean =
    positions.forall({ case (ch, positions) => positions.forall({ case (_, j) => TargetColumn(ch) == j }) })

  case class State(cost: Int, positions: Map[Char, Set[(Int, Int)]])
  object State {
    implicit val ordering: Ordering[State] = Ordering.by[State, Int](_.cost).reverse
  }

  def cost(grid: Vector[String]): Int = {
    val initialPositions = grid.zipWithIndex.foldLeft(Map.empty[Char, Set[(Int, Int)]])({ case (curr, (line, i)) =>
      line.zipWithIndex.foldLeft(curr)({ case (curr, (ch, j)) =>
        if (ch.isLetter) curr.updated(ch, curr.getOrElse(ch, Set.empty) + ((i, j)))
        else curr
      })
    })

    val emptyGrid = grid.foldLeft(Vector.empty[String])((currLines, line) =>
      currLines.appended(line.foldLeft("")((currLine, ch) => if (ch.isLetter) currLine + "." else currLine + ch))
    )

    val pq = mutable.PriorityQueue.empty[State]
    pq.enqueue(State(0, initialPositions))

    val visited = mutable.Map.empty[Map[Char, Set[(Int, Int)]], Int]
    visited.addOne((initialPositions, 0))

    var ans = -1

    while (pq.nonEmpty && ans == -1) {
      val State(currCost, currPositions) = pq.dequeue()

      val currentGrid = emptyGrid.zipWithIndex.foldLeft(Vector.empty[String])({ case (lines, (line, i)) =>
        lines.appended(line.zipWithIndex.foldLeft("")({ case (line, (ch, j)) =>
          currPositions.find(_._2((i, j))) match {
            case None                => line + ch
            case Some((amphipod, _)) => line + amphipod
          }
        }))
      })

      if (done(currPositions))
        ans = currCost
      else
        currPositions.foreach({ case (ch, positions) =>
          positions.foreach({ case (i, j) =>
            val inHallway = i == 1
            val inRoom = !inHallway
            val shouldLeaveRoom =
              inRoom && (j != TargetColumn(ch) || (j == TargetColumn(ch) && (i + 1 until emptyGrid.length).exists(ni =>
                currPositions.exists({ case (otherCh, otherPos) => ch != otherCh && otherPos((ni, j)) })
              )))

            if (shouldLeaveRoom)
              emptyGrid(1).zipWithIndex.foreach({ case (gridCh, dj) =>
                if (gridCh == '.' && emptyGrid(2)(dj) == '#') {
                  energyRequired(emptyGrid, (i, j), (1, dj), EnergyRequired(ch), currPositions).foreach(tripEnergy => {
                    val nextPositions = currPositions.updated(ch, currPositions(ch) - ((i, j)) + ((1, dj)))
                    val nextCost = currCost + tripEnergy
                    if (visited.get(nextPositions).isEmpty || visited(nextPositions) > nextCost) {
                      visited(nextPositions) = nextCost
                      pq.enqueue(State(nextCost, nextPositions))
                    }
                  })
                }
              })

            if (inHallway) {
              val canGoToRoom = currPositions.forall({ case (otherCh, pos) =>
                (ch == otherCh) || (ch != otherCh && !pos.exists(_._2 == TargetColumn(ch)))
              })

              if (canGoToRoom) {
                (emptyGrid.length - 2 to 2 by -1)
                  .flatMap(di =>
                    energyRequired(emptyGrid, (i, j), (di, TargetColumn(ch)), EnergyRequired(ch), currPositions)
                      .map(energy => (di, energy))
                  )
                  .headOption match {
                  case None => // Do nothing.
                  case Some((ti, tripEnergy)) =>
                    val nextPositions =
                      currPositions.updated(ch, currPositions(ch) - ((i, j)) + ((ti, TargetColumn(ch))))
                    val nextCost = currCost + tripEnergy
                    if (visited.get(nextPositions).isEmpty || visited(nextPositions) > nextCost) {
                      visited(nextPositions) = nextCost
                      pq.enqueue(State(nextCost, nextPositions))
                    }
                }
              }
            }
          })
        })
    }

    ans
  }

  def run(input: String): (Int, Int) = {
    val grid = input.split("\n").toVector
    val extraGrid = grid.take(3) ++ Vector("  #D#C#B#A#", "  #D#B#A#C#") ++ grid.drop(3)
    (cost(grid), cost(extraGrid))
  }
}
