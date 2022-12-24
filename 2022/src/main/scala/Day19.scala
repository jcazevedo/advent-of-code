import scala.collection.mutable

object Day19 extends DailyChallenge[Int, Int] {
  case class Blueprint(id: Int, costs: Map[String, Map[String, Int]])
  case class State(time: Int, robots: Map[String, Int], resources: Map[String, Int])

  def maxGeodes(blueprint: Blueprint, minutes: Int): Int = {
    val robots = blueprint.costs.keySet
    val cache = mutable.Map.empty[State, Int]
    var bestScore: Int = 0

    def produce(state: State): State =
      state.copy(
        time = state.time - 1,
        resources = state.robots.foldLeft(state.resources)({ case (curr, (resource, amount)) =>
          curr.updated(resource, curr.getOrElse(resource, 0) + amount)
        })
      )

    def build(state: State, robot: String): Option[State] =
      if (blueprint.costs(robot).exists({ case (resource, amount) => state.resources.getOrElse(resource, 0) < amount }))
        None
      else {
        val afterBuild = blueprint
          .costs(robot)
          .foldLeft(state.resources)({ case (curr, (resource, amount)) =>
            curr.updated(resource, curr(resource) - amount)
          })

        val afterProduction = state.robots.foldLeft(afterBuild)({ case (curr, (resource, amount)) =>
          curr.updated(resource, curr.getOrElse(resource, 0) + amount)
        })

        val nextState = state.copy(
          time = state.time - 1,
          robots = state.robots.updated(robot, state.robots.getOrElse(robot, 0) + 1),
          resources = afterProduction
        )

        Some(nextState)
      }

    def next(state: State): Set[State] = {
      val stateRobots = state.robots.values.sum
      robots.flatMap(r =>
        LazyList
          .iterate(state)(s => build(s, r).getOrElse(produce(s)))
          .drop(1)
          .takeWhile(_.time >= 0)
          .dropWhile(_.robots.values.sum == stateRobots)
          .headOption
      )
    }

    def bestOf(state: State): Int = {
      if (!cache.contains(state)) {
        val currentScore = state.robots.getOrElse("geode", 0) * state.time + state.resources.getOrElse("geode", 0)
        val upperBound = currentScore + (state.time - 1) * state.time / 2

        if (upperBound < bestScore) cache(state) = upperBound
        else {
          bestScore = math.max(bestScore, currentScore)
          cache(state) = next(state).map(bestOf).maxOption.getOrElse(currentScore)
        }
      }
      cache(state)
    }

    val start = State(minutes, robots = Map("ore" -> 1), resources = Map.empty)
    val maxGeodes = bestOf(start)

    maxGeodes
  }

  def run(input: String): (Int, Int) = {
    val blueprintRegex = "Blueprint (\\d+): (.+)".r
    val robotRegex = "Each ([^\\s]+) robot costs ([^\\.]+)\\.?".r

    val blueprints = input
      .split("\n")
      .map(line => {
        val Some(List(blueprint, robotCosts)) = (blueprintRegex.unapplySeq(line): @unchecked)
        Blueprint(
          blueprint.toInt,
          robotCosts
            .split("\\.\\s+")
            .map(costLine => {
              val Some(List(robot, costs)) = (robotRegex.unapplySeq(costLine): @unchecked)
              robot -> costs
                .split(" and ")
                .map(values => {
                  val Array(amount, resource) = values.split(" ")
                  resource -> amount.toInt
                })
                .toMap
            })
            .toMap
        )
      })
      .toList

    val part1 = blueprints.map(b => maxGeodes(b, minutes = 24) * b.id).sum
    val part2 = blueprints.take(3).map(b => maxGeodes(b, minutes = 32)).product

    (part1, part2)
  }
}
