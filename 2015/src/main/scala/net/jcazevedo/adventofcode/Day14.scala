package net.jcazevedo.adventofcode

class Day14 extends DailyChallenge[Int, Int] {
  case class Reindeer(name: String, speed: Int, activeSeconds: Int, restSeconds: Int)

  def run(filename: String): (Int, Int) = {
    val reindeers = io.Source.fromFile(filename).getLines.toList.map { line =>
      val Array(name, _, _, speed, _, _, activeSeconds, _, _, _, _, _, _, restSeconds, _) = line.split(" ")
      Reindeer(name, speed.toInt, activeSeconds.toInt, restSeconds.toInt)
    }

    def travelledDistance(reindeer: Reindeer, seconds: Int) = {
      var s = seconds
      var dist = 0
      while (s > 0) {
        if (s < reindeer.activeSeconds) {
          dist += s * reindeer.speed
          s = 0
        } else {
          dist += reindeer.activeSeconds * reindeer.speed
          s -= reindeer.activeSeconds
          s -= reindeer.restSeconds
        }
      }
      dist
    }

    def getPoints(reindeers: Seq[Reindeer], maxSeconds: Int) = {
      val active = reindeers.map(_ => true).toArray
      val count = reindeers.map(_.activeSeconds).toArray
      val dist = reindeers.map(_ => 0).toArray
      val points = reindeers.map(_ => 0).toArray
      val N = reindeers.size

      (0 until maxSeconds).foreach { _ =>
        (0 until N).foreach { i =>
          if (active(i))
            dist(i) += reindeers(i).speed

          count(i) -= 1
          if (count(i) == 0) {
            if (active(i)) {
              active(i) = false
              count(i) = reindeers(i).restSeconds
            } else {
              active(i) = true
              count(i) = reindeers(i).activeSeconds
            }
          }
        }

        val maxDist = dist.max
        (0 until N).foreach { i =>
          if (dist(i) == maxDist)
            points(i) += 1
        }
      }

      points.toList
    }

    val dist = reindeers.map(r => travelledDistance(r, 2503)).max
    val points = getPoints(reindeers, 2503).max

    (dist, points)
  }
}
