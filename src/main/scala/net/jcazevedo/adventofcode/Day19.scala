package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day19 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines().toList
    val transformations = lines.filter(_.contains("=>")).map { l =>
      val Array(from, _, to) = l.split(" ")
      (from, to)
    }.toSet
    val molecule = lines.last

    def transform(molecule: String, t: Set[(String, String)]): Set[String] = {
      val N = molecule.size

      t.flatMap {
        case (from, to) =>
          (0 until N).filter(molecule.startsWith(from, _)).map { i =>
            molecule.substring(0, i) + to + molecule.substring(i + from.size)
          }
      }
    }

    def simplify(molecule: String) = {
      molecule.replaceAll("Rn", "1").replaceAll("Y", "2").replaceAll("Ar", "3").replaceAll("[A-Z][a-z]*", "4").replaceAll("e", "4")
    }

    val simplifiedTransformations = transformations.map {
      case (k, v) =>
        simplify(k) -> simplify(v)
    }

    def dist(from: String, to: String, t: Set[(String, String)]): Int = {
      if (from == to)
        0
      else {
        1 + dist(transform(from, t).head, to, t)
      }
    }

    (transform(molecule, transformations).size,
      dist(simplify(molecule), simplify("e"), simplifiedTransformations.map { case (k, v) => v -> k }))
  }
}
