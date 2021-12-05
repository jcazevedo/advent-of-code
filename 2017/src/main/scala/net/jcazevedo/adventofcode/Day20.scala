package net.jcazevedo.adventofcode

case class Vec(x: Long, y: Long, z: Long) {
  def +(vec: Vec) = this.copy(x = this.x + vec.x, y = this.y + vec.y, z = this.z + vec.z)
}

case class Particle(p: Vec, v: Vec, a: Vec) {
  lazy val next: Particle = {
    val nextV = v + a
    val nextP = p + nextV
    this.copy(p = nextP, v = nextV)
  }

  lazy val dist = math.abs(p.x) + math.abs(p.y) + math.abs(p.z)
}

class Day20 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines().toList

    val particles: Seq[(Particle, Int)] = lines.map { line =>
      val parts = line.split(", ")
      val nn = parts.map { part =>
        part.drop(3).dropRight(1).split(",").map(_.toLong)
      }
      Particle(Vec(nn(0)(0), nn(0)(1), nn(0)(2)), Vec(nn(1)(0), nn(1)(1), nn(1)(2)), Vec(nn(2)(0), nn(2)(1), nn(2)(2)))
    }.zipWithIndex

    def simulate(particle: Particle, runs: Int = 10000): Particle = runs match {
      case 0 => particle
      case other => simulate(particle.next, other - 1)
    }

    def go1(particles: List[(Particle, Int)], runs: Int = 10000): Int = {
      particles.map(p => simulate(p._1, runs) -> p._2).minBy(_._1.dist)._2
    }

    def go2(particles: List[Particle], runs: Int = 10000): Int = {
      val particlesWithoutCollisions = particles.groupBy(_.p).filter(_._2.length == 1).map(_._2.head).toList
      runs match {
        case 0 => particlesWithoutCollisions.size
        case _ => go2(particlesWithoutCollisions.map(_.next), runs - 1)
      }
    }

    val res1 = go1(particles.toList)
    val res2 = go2(particles.toList.map(_._1))

    (res1, res2)
  }
}
