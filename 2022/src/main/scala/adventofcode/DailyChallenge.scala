package adventofcode

import scala.io.Source

trait DailyChallenge[P1, P2] {
  def run(input: String): (P1, P2)

  def main(args: Array[String]): Unit = {
    val className = getClass().getSimpleName()
    val inputFile = s"input/${className.drop(3).dropRight(1)}.input"
    val source = Source.fromFile(inputFile)
    val input = source.mkString
    source.close()
    val res = run(input)
    println(s"Part 1: ${res._1}")
    println(s"Part 2: ${res._2}")
  }
}
