package net.jcazevedo.adventofcode

class Day18 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    var grid = io.Source.fromFile(filename).getLines.toList.map { line =>
      line.map { c =>
        if (c == '#') true else false
      }.toArray
    }.toArray

    val n = Array((-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1))

    def step(g: Array[Array[Boolean]]): Array[Array[Boolean]] = {
      val h = g.size
      val w = g.size

      val nextGrid = Array.ofDim[Boolean](h, w)
      (0 until h).foreach { i =>
        (0 until w).foreach { j =>
          val lightStatus = g(i)(j)
          val neighborsOn = n.count {
            case (di, dj) =>
              val ni = i + di
              val nj = j + dj
              ni >= 0 && ni < h && nj >= 0 && nj < w && g(ni)(nj)
          }

          if (lightStatus) {
            if (neighborsOn == 2 || neighborsOn == 3)
              nextGrid(i)(j) = true
            else
              nextGrid(i)(j) = false
          } else {
            if (neighborsOn == 3)
              nextGrid(i)(j) = true
            else
              nextGrid(i)(j) = false
          }
        }
      }
      nextGrid
    }

    def step2(g: Array[Array[Boolean]]): Array[Array[Boolean]] = {
      val h = g.size
      val w = g.size

      val nextGrid = Array.ofDim[Boolean](h, w)
      (0 until h).foreach { i =>
        (0 until w).foreach { j =>
          val lightStatus = g(i)(j)
          val neighborsOn = n.count {
            case (di, dj) =>
              val ni = i + di
              val nj = j + dj
              ni >= 0 && ni < h && nj >= 0 && nj < w && g(ni)(nj)
          }

          if (lightStatus) {
            if (neighborsOn == 2 || neighborsOn == 3)
              nextGrid(i)(j) = true
            else
              nextGrid(i)(j) = false
          } else {
            if (neighborsOn == 3)
              nextGrid(i)(j) = true
            else
              nextGrid(i)(j) = false
          }
        }
      }

      nextGrid(0)(0) = true
      nextGrid(0)(w - 1) = true
      nextGrid(h - 1)(0) = true
      nextGrid(h - 1)(w - 1) = true

      nextGrid
    }

    var grid2 = grid.clone()

    (0 until 100).foreach { _ => grid = step(grid) }
    val lightsOn = grid.map(_.count(identity)).sum
    grid2(0)(0) = true
    grid2(0)(grid2(0).size - 1) = true
    grid2(grid2.size - 1)(0) = true
    grid2(grid2.size - 1)(grid2(0).size - 1) = true
    (0 until 100).foreach { _ => grid2 = step2(grid2) }
    val lightsOn2 = grid2.map(_.count(identity)).sum

    (lightsOn, lightsOn2)
  }
}
