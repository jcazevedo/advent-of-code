package net.jcazevedo.adventofcode

trait DailyChallenge[P1, P2] {
  def run(filename: String): (P1, P2)
}

object DailyChallengeRunner extends App {
  val cls = args(0)
  val challenge =
    getClass.getClassLoader.loadClass("net.jcazevedo.adventofcode." + cls)
      .newInstance().asInstanceOf[DailyChallenge[_, _]]
  val input = args(0).drop(3) + ".input"
  println("Running challenge for " + cls + "...")
  val res = challenge.run(input)
  println("Part 1: " + res._1)
  println("Part 2: " + res._2)
}
