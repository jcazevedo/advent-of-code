package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day13 extends DailyChallenge[Int, Int] {
  def isWall(pos: (Int, Int), favNumber: Int): Boolean = {
    val (x, y) = pos
    if (x < 0 || y < 0)
      true
    else {
      val s = x * x + 3 * x + 2 * x * y + y + y * y + favNumber
      val nBits = (0 until 31).map { d =>
        if ((s & (1l << d)) == 0) 0 else 1
      }.sum
      (nBits % 2) != 0
    }
  }

  def go(start: (Int, Int), end: (Int, Int), favNumber: Int): Int = {
    implicit val ordering: Ordering[(Int, (Int, Int))] =
      Ordering.by[(Int, (Int, Int)), Int](_._1).reverse

    val dirs = Seq((1, 0), (-1, 0), (0, 1), (0, -1))
    val visited = mutable.Set[(Int, Int)](start)

    val pq = mutable.PriorityQueue[(Int, (Int, Int))]()
    pq += ((0, start))

    while (!pq.isEmpty) {
      val (dist, (x, y)) = pq.dequeue()

      if ((x, y) == end)
        return dist

      dirs.foreach {
        case (dx, dy) =>
          val next = (x + dx, y + dy)
          if (!visited.contains(next) && !isWall(next, favNumber)) {
            pq += ((dist + 1, next))
            visited += next
          }
      }
    }

    -1
  }

  def go1(start: (Int, Int), favNumber: Int): Int = {
    val dirs = Seq((1, 0), (-1, 0), (0, 1), (0, -1))
    val visited = mutable.Set[(Int, Int)](start)
    val positions = mutable.Set[(Int, Int)]()
    val q = mutable.Queue[(Int, (Int, Int))]()
    q += ((0, start))

    while (!q.isEmpty) {
      val (dist, (x, y)) = q.dequeue()

      if (dist <= 50) {
        positions += ((x, y))
        dirs.foreach {
          case (dx, dy) =>
            val next = (x + dx, y + dy)
            if (!visited.contains(next) && !isWall(next, favNumber)) {
              q += ((dist + 1, next))
              visited += next
            }
        }
      }
    }

    positions.size
  }

  def run(filename: String): (Int, Int) = {
    val favNumber = io.Source.fromFile(filename).getLines.mkString.toInt
    (go((1, 1), (31, 39), favNumber), go1((1, 1), favNumber))
  }
}
