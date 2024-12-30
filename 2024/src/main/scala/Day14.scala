import scala.io.StdIn

object Day14 extends DailyChallenge[Long, String] {
  case class Robot(px: Long, py: Long, vx: Long, vy: Long)

  def mod(a: Long, b: Long): Long =
    if (a > 0) a % b
    else (a % b + b) % b

  def run(input: String): (Long, String) = {
    val H = 103
    val W = 101
    val robotRegex = """p=([+-]?[0-9]+),([+-]?[0-9]+) v=([+-]?[0-9]+),([+-]?[0-9]+)""".r

    def simulate(robot: Robot, seconds: Int): (Long, Long) = {
      val x = mod(robot.px + seconds * robot.vx, W)
      val y = mod(robot.py + seconds * robot.vy, H)
      (x, y)
    }

    val robots = robotRegex
      .findAllIn(input)
      .collect({ case robotRegex(px, py, vx, vy) => Robot(px.toLong, py.toLong, vx.toLong, vy.toLong) })
      .toList

    def printAfter(seconds: Int): String = {
      val positions = robots.map(simulate(_, seconds))

      val grid = Array.fill(H)(Array.fill(W)('.'))
      positions.foreach((x, y) => grid(y.toInt)(x.toInt) = '*')

      grid.map(_.mkString).mkString("\n")
    }

    val part1 = {
      val after100 = robots.map(simulate(_, 100))
      val tl = after100.count((x, y) => x < W / 2 && y < H / 2).toLong
      val tr = after100.count((x, y) => x > W / 2 && y < H / 2).toLong
      val bl = after100.count((x, y) => x < W / 2 && y > H / 2).toLong
      val br = after100.count((x, y) => x > W / 2 && y > H / 2).toLong

      tl * tr * bl * br
    }

    // I was not sure how the tree would like, so I started filtering out for
    // representations in which a lot of cells had a lot of neighbors.
    //
    // (0 until 10000000).iterator
    //   .filter(i => {
    //     val positions = robots.map(simulate(_, i)).toSet
    //     val moreThan2 = positions
    //       .map((x, y) => {
    //         for {
    //           dx <- (-1 to 1)
    //           dy <- (-1 to 1)
    //           if dx != 0 || dy != 0
    //           if positions.contains((x + dx, y + dy))
    //         } yield (x + dx, y + dy)
    //       })
    //       .count(_.size > 2)
    //     moreThan2 > 50
    //   })
    //   .foreach(i => {
    //     println(i)
    //     println(printAfter(i))
    //     StdIn.readLine()
    //   })
    val part2 = s"\n${printAfter(7569)}"

    (part1, part2)
  }
}
