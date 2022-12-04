object Day17 extends DailyChallenge[Int, Int] {
  case class Range(from: Int, to: Int)
  case class Area(xRange: Range, yRange: Range)

  def getHighestY(area: Area): Int = {
    // We can always get there in X, and we can't go higher than the max Y, since when the probe crosses the Y axis, it
    // reverses the initial velocity.
    (0 to math.abs(area.yRange.from))
      .flatMap(initialV => {
        var currY = 0
        var vy = initialV
        var maxY = 0
        while (currY > area.yRange.to) {
          currY += vy
          vy -= 1
          maxY = math.max(maxY, currY)
        }
        if (currY >= area.yRange.from) Some(maxY)
        else None
      })
      .max
  }

  def getGoodInitialVelocities(area: Area): Set[(Int, Int)] = {
    def good(vx: Int, vy: Int): Boolean = {
      var currX = 0
      var currY = 0
      var currVX = vx
      var currVY = vy
      var good = false
      while (currY >= area.yRange.from && currX <= area.xRange.to) {
        currX += currVX
        currY += currVY
        currVX = if (currVX == 0) 0 else (currVX - 1)
        currVY -= 1
        good =
          good || currX >= area.xRange.from && currX <= area.xRange.to && currY <= area.yRange.to && currY >= area.yRange.from
      }
      good
    }

    // Brute-force because we can!
    for {
      vx <- (0 to area.xRange.to).toSet
      vy <- (area.yRange.from to -area.yRange.from).toSet
      if good(vx, vy)
    } yield (vx, vy)
  }

  def run(input: String): (Int, Int) = {
    val lines = input.split("\n").toList

    val area = {
      val Array(xr, yr) = lines.head.stripPrefix("target area: ").split(", ")
      val Array(xf, xt) = xr.stripPrefix("x=").split("\\.\\.")
      val Array(yf, yt) = yr.stripPrefix("y=").split("\\.\\.")
      Area(Range(xf.toInt, xt.toInt), Range(yf.toInt, yt.toInt))
    }

    val part1 = getHighestY(area)
    val part2 = getGoodInitialVelocities(area).size

    (part1, part2)
  }
}
