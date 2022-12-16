import scala.collection.mutable

object Day15 extends DailyChallenge[Int, Long] {
  case class Point(x: Int, y: Int)
  case class Interval(from: Int, to: Int)
  case class SensorReading(sensor: Point, beacon: Point)

  case class IntervalList(intervals: Vector[Interval]) {
    def add(interval: Interval): IntervalList = {
      def merge(intervals: Vector[Interval]): Vector[Interval] =
        intervals match {
          case Interval(fa, ta) +: Interval(fb, tb) +: t if ta >= fb =>
            merge(Interval(fa, math.max(ta, tb)) +: t)
          case h +: t =>
            h +: merge(t)
          case _ =>
            Vector.empty
        }

      IntervalList({
        val (a, b) = intervals.span(_.from <= interval.from)
        merge(a ++ (interval +: b))
      })
    }

    def totalLength: Int =
      intervals.map(interval => interval.to - interval.from + 1).sum

    def trim(from: Int, to: Int): IntervalList =
      IntervalList(
        intervals
          .filter(interval => from <= interval.to && interval.from <= to)
          .map(interval => Interval(math.max(from, interval.from), math.min(to, interval.to)))
      )
  }
  object IntervalList {
    final val empty: IntervalList = IntervalList(Vector.empty)
  }

  def getScan(readings: List[SensorReading]): Map[Int, IntervalList] = {
    val grid = mutable.Map.empty[Int, IntervalList]

    readings.foreach { case SensorReading(Point(sx, sy), Point(bx, by)) =>
      val manhattanDistance = math.abs(bx - sx) + math.abs(by - sy)
      (0 to manhattanDistance).foreach { dist =>
        val interval = Interval(sx - (manhattanDistance - dist), sx + (manhattanDistance - dist))
        grid.updateWith(sy - dist)(_.map(_.add(interval)).orElse(Some(IntervalList(Vector(interval)))))
        grid.updateWith(sy + dist)(_.map(_.add(interval)).orElse(Some(IntervalList(Vector(interval)))))
      }
    }

    grid.toMap.withDefaultValue(IntervalList.empty)
  }

  def run(input: String): (Int, Long) = {
    val readingRegex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

    val readings = input
      .split("\n")
      .map(line => {
        val Some(List(sx, sy, bx, by)) = (readingRegex.unapplySeq(line): @unchecked)
        SensorReading(Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))
      })
      .toList

    val grid = getScan(readings)

    val part1 = {
      val y = 2000000

      grid(y).totalLength - readings
        .map(_.beacon)
        .toSet
        .count(b => b.y == y && grid(y).intervals.exists(i => b.x >= i.from && b.x <= i.to))
    }

    val part2 = {
      val minV = 0
      val maxV = 4000000

      val (x, y) = (for {
        y <- minV to maxV
        trimmed = grid(y).trim(minV, maxV).intervals
        if trimmed.length > 1 || trimmed.length == 0 || trimmed.head.from > minV || trimmed.head.to < maxV
        x <- minV to maxV
        if !trimmed.exists(interval => x >= interval.from && x <= interval.to)
      } yield (x, y)).head

      x * 4000000L + y
    }

    (part1, part2)
  }
}
