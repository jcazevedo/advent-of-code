import scala.util.parsing.combinator.RegexParsers
import scala.math.Ordering.Implicits._

object Day13 extends DailyChallenge[Int, Int] {
  sealed trait Packet
  case class PInteger(value: Int) extends Packet
  case class PList(value: List[Packet]) extends Packet

  object Packet {
    def parse(str: String): Packet = PacketParser.parse(PacketParser.packet, str).get

    implicit val packetOrdering: Ordering[Packet] = Ordering.fromLessThan {
      case (PInteger(v1), PInteger(v2)) => v1 < v2
      case (PList(v1), PList(v2)) =>
        val zipped = v1.zip(v2)
        val afterEquals = zipped.dropWhile({ case (a, b) => !(a < b) && !(b < a) })
        if (afterEquals.isEmpty) v1.length < v2.length
        else afterEquals.head._1 < afterEquals.head._2
      case (v1: PInteger, v2) => (PList(List(v1)): Packet) < v2
      case (v1, v2: PInteger) => v1 < (PList(List(v2)): Packet)
    }
  }

  object PacketParser extends RegexParsers {
    def pinteger: Parser[PInteger] = """(0|[1-9]\d*)""".r ^^ (v => PInteger(v.toInt))
    def plist: Parser[PList] = "[" ~> opt(packet ~ rep("," ~> packet)) <~ "]" ^^ {
      case Some(hp ~ tp) => PList(hp :: tp)
      case None          => PList(List.empty)
    }
    def packet: Parser[Packet] = pinteger | plist
  }

  def run(input: String): (Int, Int) = {
    val packets = input.split("\n").map(_.trim).filter(_.nonEmpty).toList.map(Packet.parse)
    val packetPairs = packets.sliding(2, 2).toList

    val divider1 = Packet.parse("[[2]]")
    val divider2 = Packet.parse("[[6]]")
    val distressSignal = (divider1 :: divider2 :: packets).sorted

    val part1 = packetPairs.zipWithIndex.filter(p => p._1.sorted == p._1).map(_._2 + 1).sum
    val part2 = (distressSignal.indexOf(divider1) + 1) * (distressSignal.indexOf(divider2) + 1)

    (part1, part2)
  }
}
