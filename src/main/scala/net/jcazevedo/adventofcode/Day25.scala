package net.jcazevedo.adventofcode

class Day25 extends DailyChallenge[Int, Unit] {
  case class NextOp(valueToWrite: Int, direction: Int, nextStateId: String)
  case class State(nextOps: Map[Int, NextOp])
  case class Blueprint(states: Map[String, State])

  case class TuringMachine(tape: Map[Int, Int], cursor: Int, currentStateId: String, blueprint: Blueprint) {
    def step: TuringMachine = {
      val nextOp = blueprint.states(currentStateId).nextOps(tape.getOrElse(cursor, 0))
      this.copy(
        tape = tape + (cursor -> nextOp.valueToWrite),
        cursor = cursor + nextOp.direction,
        currentStateId = nextOp.nextStateId)
    }
  }

  def run(filename: String): (Int, Unit) = {
    val lines = io.Source.fromFile(filename).getLines().toList
    val initialState = lines(0).stripPrefix("Begin in state ").stripSuffix(".")
    val stepsForChecksum = lines(1).stripPrefix("Perform a diagnostic checksum after ").stripSuffix(" steps.").toInt

    val states = lines.drop(2).sliding(10, 10).map { ll =>
      val stateName = ll(1).stripPrefix("In state ").stripSuffix(":")
      val nextOps = List(0, 1).map { v =>
        val valueToWrite = ll(v * 4 + 3).stripPrefix("    - Write the value ").stripSuffix(".").toInt
        val direction = if (ll(v * 4 + 4).stripPrefix("    - Move one slot to the ").stripSuffix(".") == "right") 1 else -1
        val nextStateId = ll(v * 4 + 5).stripPrefix("    - Continue with state ").stripSuffix(".")
        v -> NextOp(valueToWrite, direction, nextStateId)
      }.toMap
      stateName -> State(nextOps)
    }.toMap

    val blueprint = Blueprint(states)
    val machine = TuringMachine(Map.empty, 0, initialState, blueprint)
    val machineIt = Iterator.iterate(machine)(_.step)
    val res1 = machineIt.drop(stepsForChecksum).next.tape.valuesIterator.count(_ == 1)

    (res1, ())
  }
}
