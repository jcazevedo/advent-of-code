package net.jcazevedo.adventofcode

case class Present(l: Int, w: Int, h: Int)

object Present {
  def apply(s: String): Present = {
    val Array(l, w, h) = s.split('x').map(_.toInt)
    Present(l, w, h)
  }
}

class Day02 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def getPaper(p: Present): Int = {
      val surfaces = Seq(p.l * p.w, p.w * p.h, p.h * p.l)
      surfaces.sum * 2 + surfaces.min
    }

    def getRibbon(p: Present): Int = {
      val perimiters = Seq(p.l * 2 + p.w * 2, p.w * 2 + p.h * 2, p.h * 2 + p.l * 2)
      perimiters.min + p.l * p.w * p.h
    }

    val presents = io.Source.fromFile(filename).getLines.toList.map(Present.apply)
    val paper = presents.map(getPaper).sum
    val ribbon = presents.map(getRibbon).sum

    (paper, ribbon)
  }
}
