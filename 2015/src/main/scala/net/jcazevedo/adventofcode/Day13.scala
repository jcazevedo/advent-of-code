package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day13 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val happinessLevels = mutable.Map[(String, String), Int]().withDefaultValue(0)
    val people = mutable.Set[String]()

    io.Source.fromFile(filename).getLines.foreach { line =>
      val Array(n1, _, s, v, _, _, _, _, _, _, nt2) = line.split(" ")
      val n2 = nt2.substring(0, nt2.size - 1)
      val h = if (s == "lose") -v.toInt else v.toInt
      people += n1
      happinessLevels((n1, n2)) = h
    }

    def getMaxHappiness(people: Set[String]) = {
      people.toList.sorted.permutations.map { perm =>
        var h = 0
        val N = people.size
        (0 until N).foreach { i =>
          val nn = (i + 1) % N
          val pn = (i + N - 1) % N
          h += happinessLevels((perm(i), perm(nn)))
          h += happinessLevels((perm(i), perm(pn)))
        }
        h
      }.max
    }

    val h1 = getMaxHappiness(people.toSet)
    val h2 = getMaxHappiness(people.toSet + "Me")

    (h1, h2)
  }
}
