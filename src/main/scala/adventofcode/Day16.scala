package adventofcode

import scala.collection.mutable

class Day16 extends DailyChallenge[Int, Long] {
  case class Range(min: Int, max: Int) {
    def inRange(value: Int): Boolean = value >= min && value <= max
  }
  case class Field(name: String, constraints: List[Range]) {
    def valid(value: Int): Boolean = constraints.exists(_.inRange(value))
  }
  case class Ticket(fields: List[Int])

  def validField(fields: List[Field], value: Int): Boolean = fields.exists(_.valid(value))

  def part1(fields: List[Field], tickets: List[Ticket]): Int =
    tickets.flatMap(_.fields.filter(!validField(fields, _))).sum

  def part2(fields: List[Field], yourTicket: Ticket, nearbyTickets: List[Ticket]): Long = {
    val validTickets = nearbyTickets.filter(_.fields.forall(validField(fields, _)))

    val fieldOptions =
      validTickets.map(_.fields.zipWithIndex).foldRight(Vector.fill(validTickets.head.fields.length)(List.empty[Int])) {
        case (fields, vec) => fields.foldLeft(vec) { case (vec, (value, idx)) => vec.updated(idx, value :: vec(idx)) }
      }

    val fieldsV = fields.toVector

    val graph: Vector[List[Int]] =
      fieldOptions.map(fieldValues => fieldsV.indices.filter(idx => fieldValues.forall(fieldsV(idx).valid)).toList)

    val INF = Int.MaxValue
    val NIL = fieldOptions.length

    val matchesL = mutable.ListBuffer.fill(fieldOptions.length + 1)(NIL)
    val matchesR = mutable.ListBuffer.fill(graph.length + 1)(NIL)
    val dist = mutable.ListBuffer.fill(fieldOptions.length + 1)(NIL)

    def bfs(): Boolean = {
      val q = mutable.Queue[Int]()
      graph.indices.foreach { l =>
        if (matchesL(l) == NIL) {
          dist(l) = 0
          q.enqueue(l)
        } else {
          dist(l) = INF
        }
      }
      dist(NIL) = INF
      while (q.nonEmpty) {
        val l = q.dequeue()
        if (dist(l) < dist(NIL)) {
          graph(l).foreach { r =>
            if (dist(matchesR(r)) == INF) {
              dist(matchesR(r)) = dist(l) + 1
              q.enqueue(matchesR(r))
            }
          }
        }
      }
      dist(NIL) != INF
    }

    def dfs(l: Int): Boolean = {
      if (l == NIL) true
      else
        graph(l).find { r => dist(matchesR(r)) == dist(l) + 1 && dfs(matchesR(r)) } match {
          case Some(r) =>
            matchesR(r) = l
            matchesL(l) = r
            true

          case None =>
            dist(l) = INF
            false
        }
    }

    var ans = 0
    while (bfs()) {
      graph.indices.foreach { l => if (matchesL(l) == NIL && dfs(l)) ans += 1 }
    }

    assert(ans == graph.length)

    val assigned = matchesL.take(graph.length).map(fieldsV.apply)
    yourTicket.fields.zip(assigned).filter(_._2.name.startsWith("departure")).map(_._1.toLong).product
  }

  def run(input: String): (Int, Long) = {
    val fieldRegex = raw"([^:]+): (.*)".r
    val (fields, yourTicket, nearbyTickets) =
      input.split("\n").foldLeft((List.empty[Field], Option.empty[Ticket], List.empty[Ticket])) {
        case (acc, "your ticket:")    => acc
        case (acc, "nearby tickets:") => acc
        case (acc, "")                => acc
        case ((f, yt, nt), fieldRegex(name, constraints)) =>
          val nextField = Field(
            name,
            constraints.split(" or ").toList.map { str =>
              val Array(min, max) = str.split("-").map(_.toInt)
              Range(min, max)
            }
          )
          (f :+ nextField, yt, nt)
        case ((f, None, nt), ticketS) =>
          val ticket = Ticket(ticketS.split(",").map(_.toInt).toList)
          (f, Some(ticket), nt)
        case ((f, Some(yt), nt), ticketS) =>
          val ticket = Ticket(ticketS.split(",").map(_.toInt).toList)
          (f, Some(yt), nt :+ ticket)
      }
    assert(yourTicket.nonEmpty)

    (part1(fields, nearbyTickets), part2(fields, yourTicket.get, nearbyTickets))
  }
}
