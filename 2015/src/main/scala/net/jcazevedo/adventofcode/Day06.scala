package net.jcazevedo.adventofcode

class Day06 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val s = 1000
    val grid = Array.ofDim[Boolean](s, s)
    val grid2 = Array.ofDim[Int](s, s)

    def on(v: Boolean) = true
    def off(v: Boolean) = false
    def toggle(v: Boolean) = !v

    def on2(v: Int) = v + 1
    def off2(v: Int) = if (v > 0) v - 1 else v
    def toggle2(v: Int) = v + 2

    def apply(from: (Int, Int), to: (Int, Int), f: (Boolean => Boolean)) {
      (from._1 to to._1).foreach { i =>
        (from._2 to to._2).foreach { j =>
          grid(i)(j) = f(grid(i)(j))
        }
      }
    }

    def apply2(from: (Int, Int), to: (Int, Int), f: (Int => Int)) {
      (from._1 to to._1).foreach { i =>
        (from._2 to to._2).foreach { j =>
          grid2(i)(j) = f(grid2(i)(j))
        }
      }
    }

    val instructions = io.Source.fromFile(filename).getLines
    instructions.foreach { instruction =>
      if (instruction.startsWith("turn on")) {
        val Array(_, _, from, _, to) = instruction.split(" ")
        val Array(fromI, fromJ) = from.split(',').map(_.toInt)
        val Array(toI, toJ) = to.split(',').map(_.toInt)
        apply((fromI, fromJ), (toI, toJ), on)
        apply2((fromI, fromJ), (toI, toJ), on2)
      } else if (instruction.startsWith("turn off")) {
        val Array(_, _, from, _, to) = instruction.split(" ")
        val Array(fromI, fromJ) = from.split(',').map(_.toInt)
        val Array(toI, toJ) = to.split(',').map(_.toInt)
        apply((fromI, fromJ), (toI, toJ), off)
        apply2((fromI, fromJ), (toI, toJ), off2)
      } else if (instruction.startsWith("toggle")) {
        val Array(_, from, _, to) = instruction.split(" ")
        val Array(fromI, fromJ) = from.split(',').map(_.toInt)
        val Array(toI, toJ) = to.split(',').map(_.toInt)
        apply((fromI, fromJ), (toI, toJ), toggle)
        apply2((fromI, fromJ), (toI, toJ), toggle2)
      }
    }

    (grid.map(_.count(identity)).sum, grid2.map(_.sum).sum)
  }
}
