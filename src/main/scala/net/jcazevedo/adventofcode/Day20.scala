package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day20 extends DailyChallenge[Long, Long] {
  sealed trait Event
  case object In extends Event
  case object Out extends Event

  def minIp(intervals: Seq[(Long, Long)]): Long = {
    implicit val ordering: Ordering[(Long, Event)] =
      Ordering.by[(Long, Event), Long](_._1).reverse

    var prev = -1l
    var current = 0

    val pq = mutable.PriorityQueue[(Long, Event)]()
    intervals.foreach {
      case (s, e) =>
        pq += ((s, In))
        pq += ((e, Out))
    }

    while (!pq.isEmpty) {
      val (t, ev) = pq.dequeue()

      if (t != (prev + 1) && current == 0)
        return (prev + 1)

      prev = t
      ev match {
        case In => current += 1
        case Out => current -= 1
      }
    }

    -1
  }

  def totIps(intervals: Seq[(Long, Long)]): Long = {
    implicit val ordering: Ordering[(Long, Event)] =
      Ordering.by[(Long, Event), Long](_._1).reverse

    var prev = -1l
    var current = 0
    var tot = 0l
    var max = 1l << 32

    val pq = mutable.PriorityQueue[(Long, Event)]()
    intervals.foreach {
      case (s, e) =>
        pq += ((s, In))
        pq += ((e, Out))
    }

    while (!pq.isEmpty) {
      val (t, ev) = pq.dequeue()

      if (t != (prev + 1) && current == 0) {
        tot += (t - (prev + 1))
      }

      prev = t
      ev match {
        case In => current += 1
        case Out => current -= 1
      }
    }

    tot + (max - (prev + 1))
  }

  def run(filename: String): (Long, Long) = {
    val blacklist = io.Source.fromFile(filename).getLines.toList
    val intervals = blacklist.map { s =>
      val ss = s.split("-")
      (ss(0).toLong, ss(1).toLong)
    }
    (minIp(intervals), totIps(intervals))
  }
}
