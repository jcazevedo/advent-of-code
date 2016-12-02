package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day20 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val input = io.Source.fromFile(filename).getLines.mkString.toInt

    def primeFactors(n: Int): Seq[Int] = {
      val factors = mutable.ListBuffer[Int]()
      var t = n
      while (t > 0 && t % 2 == 0) {
        factors += 2
        t /= 2
      }

      var i = 3
      while (i * i <= t) {
        while (t > 0 && t % i == 0) {
          factors += i
          t /= i
        }

        i += 2
      }

      if (t > 1)
        factors += t

      factors
    }

    def presentsOfHouse(i: Int): Int = {
      val factors = 1 +: primeFactors(i)
      (1 to factors.size)
        .flatMap(factors.combinations(_))
        .map(_.reduceLeft(_ * _))
        .toSet.map({ v: Int => v * 10 }).sum
    }

    def presentsOfHouseNotInfinte(i: Int): Int = {
      val factors = 1 +: primeFactors(i)
      (1 to factors.size)
        .flatMap(factors.combinations(_))
        .map(_.reduceLeft(_ * _))
        .toSet.filter(i / _ <= 50).map(_ * 11).sum
    }

    def getHouse(until: Int, h: Int = 1): Int = {
      if (presentsOfHouse(h) >= until)
        h
      else
        getHouse(until, h + 1)
    }

    def getHouseNotInfinite(until: Int, h: Int = 1): Int = {
      if (presentsOfHouseNotInfinte(h) >= until)
        h
      else
        getHouseNotInfinite(until, h + 1)
    }

    (getHouse(input), getHouseNotInfinite(input))
  }
}
