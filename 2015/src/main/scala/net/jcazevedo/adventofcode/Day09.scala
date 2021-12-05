package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day09 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val distances = mutable.Map[(String, String), Int]()
    val locations = mutable.Set[String]()

    val lines = io.Source.fromFile(filename).getLines.toList
    lines.foreach { line =>
      val Array(from, _, to, _, dist) = line.split(" ")
      distances((from, to)) = dist.toInt
      distances((to, from)) = dist.toInt
      locations += from
      locations += to
    }

    val lengths = locations.toList.sorted.permutations.toList.map { path =>
      path.sliding(2).map {
        case from :: to :: Nil =>
          distances(from, to)
        case _ => 0
      }.sum
    }

    val minLength = lengths.min
    val maxLength = lengths.max

    (minLength, maxLength)
  }
}
