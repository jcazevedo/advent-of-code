package adventofcode

import scala.io.Source

trait DailyChallenge[P1, P2] {
  def run(input: String): (P1, P2)
}

object DailyChallengeRunner extends App {
  val cls = args(0)
  val challenge =
    getClass.getClassLoader
      .loadClass("adventofcode." + cls)
      .getDeclaredConstructor()
      .newInstance()
      .asInstanceOf[DailyChallenge[_, _]]
  val inputFile = s"input/${args(0).drop(3)}.input"
  val source = Source.fromFile(inputFile)
  val input = source.mkString
  source.close()
  println(s"Running challenge for $cls...")
  val res = challenge.run(input)
  println(s"Part 1: ${res._1}")
  println(s"Part 2: ${res._2}")
}
