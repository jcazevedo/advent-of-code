package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day11 extends DailyChallenge[Int, Int] {
  sealed trait Component {
    def name: String
  }
  case class Microchip(name: String) extends Component
  case class Generator(name: String) extends Component

  case class State(elevatorPos: Int, floors: IndexedSeq[Set[Component]]) {
    def valid: Boolean = {
      floors.forall { floor =>
        val microchips = floor.collect({ case c: Microchip => c })
        val generators = floor.collect({ case c: Generator => c })
        microchips.forall { microchip =>
          generators.isEmpty || generators.exists {
            generator => generator.name == microchip.name
          }
        }
      }
    }

    def next: List[State] = {
      if (floors(elevatorPos).isEmpty)
        List.empty
      else {
        val nextElevatorPos = List(elevatorPos - 1, elevatorPos + 1)
          .filter(x => x >= 0 && x < floors.length)
        val nextElevatorContents = floors(elevatorPos)
          .subsets.filter(s => s.size >= 1 && s.size <= 2).toList
        val s = for {
          nextPos <- nextElevatorPos
          nextContents <- nextElevatorContents
        } yield State(
          nextPos,
          floors
            .updated(nextPos, floors(nextPos) ++ nextContents)
            .updated(elevatorPos, floors(elevatorPos) -- nextContents))
        s.filter(_.valid)
      }
    }

    def print: Unit = {
      floors.zipWithIndex.reverse.foreach {
        case (floor, idx) =>
          val elev = if (elevatorPos == idx) "E" else "."
          println(s"F$idx: $elev  $floor")
      }
      println()
    }
  }

  def go(start: State, target: State => Boolean): Int = {
    def heuristic(state: State) = {
      val nFloors = state.floors.length
      state.floors.zipWithIndex.map {
        case (floor, idx) =>
          (nFloors - idx - 1) * floor.size
      }.sum
    }

    implicit val ordering: Ordering[(Int, Int, State)] =
      Ordering.by[(Int, Int, State), Int] {
        case (dist, heur, state) =>
          dist + heur
      }.reverse

    val types = start.floors.flatMap(_.map(_.name)).toSet.toList.sorted.zipWithIndex.toMap
    val visited = mutable.Set[State](start)

    val pq = mutable.PriorityQueue[(Int, Int, State)]()
    pq += ((0, heuristic(start), start))

    while (!pq.isEmpty) {
      val (dist, heur, current) = pq.dequeue()

      if (target(current))
        return dist

      current.next.foreach { nextState =>
        if (!visited.contains(nextState)) {
          pq += ((dist + 1, heuristic(nextState), nextState))
          visited += nextState
        }
      }
    }

    -1
  }

  def run(filename: String): (Int, Int) = {
    // input is cumbersome to read, so I'm taking a shortcut here and already encoding it
    val start1 = State(
      0,
      IndexedSeq(
        Set(Generator("polonium"),
          Generator("thulium"),
          Microchip("thulium"),
          Generator("promethium"),
          Generator("ruthenium"),
          Microchip("ruthenium"),
          Generator("cobalt"),
          Microchip("cobalt")),
        Set(Microchip("polonium"),
          Microchip("promethium")),
        Set(),
        Set()))

    val start2 = State(
      0,
      IndexedSeq(
        Set(Generator("polonium"),
          Generator("thulium"),
          Microchip("thulium"),
          Generator("promethium"),
          Generator("ruthenium"),
          Microchip("ruthenium"),
          Generator("cobalt"),
          Microchip("cobalt"),
          Generator("elerium"),
          Microchip("elerium"),
          Generator("dilithium"),
          Microchip("dilithium")),
        Set(Microchip("polonium"),
          Microchip("promethium")),
        Set(),
        Set()))

    val target: State => Boolean = { state =>
      state.elevatorPos == 3 && state.floors.zipWithIndex.forall {
        case (floor, idx) =>
          if (idx < 3)
            floor.size == 0
          else
            floor.size > 0
      }
    }

    (go(start1, target), go(start2, target))
  }
}
