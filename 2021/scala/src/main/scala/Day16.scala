import scala.collection.mutable

object Day16 extends DailyChallenge[Int, Long] {
  sealed trait Packet {
    def version: Int
    def typeId: Int
  }

  case class LiteralValue(version: Int, value: Long) extends Packet {
    val typeId: Int = 4
  }

  case class Operator(version: Int, typeId: Int, subPackets: List[Packet]) extends Packet

  def toBinary(ch: Char): String = ch match {
    case '0' => "0000"
    case '1' => "0001"
    case '2' => "0010"
    case '3' => "0011"
    case '4' => "0100"
    case '5' => "0101"
    case '6' => "0110"
    case '7' => "0111"
    case '8' => "1000"
    case '9' => "1001"
    case 'A' => "1010"
    case 'B' => "1011"
    case 'C' => "1100"
    case 'D' => "1101"
    case 'E' => "1110"
    case 'F' => "1111"
  }

  def toNum(binaryStr: String): Long = {
    var res = 0L
    var curr = binaryStr

    while (curr.nonEmpty) {
      res = res * 2L + (curr.head - '0')
      curr = curr.tail
    }

    res
  }

  def parseInBits(bits: String): (Packet, Int) = {
    var str = bits
    var consumed = 0

    def advance(nBits: Int): String = {
      val res = str.take(nBits)
      str = str.drop(nBits)
      consumed += nBits
      res
    }

    val v = toNum(advance(3)).toInt
    val t = toNum(advance(3)).toInt

    if (t == 4) {
      // Literal value.
      var num = 0L
      var done = false
      while (!done) {
        val nextFive = advance(5)
        num = num * 16L + toNum(nextFive.tail)
        if (nextFive.head == '0')
          done = true
      }
      (LiteralValue(v, num), consumed)
    } else {
      // Operator.
      if (advance(1) == "0") {
        val length = toNum(advance(15)).toInt
        var subConsumed = 0
        val subPackets = mutable.ListBuffer.empty[Packet]
        while (subConsumed < length) {
          val (nextPacket, nextConsumed) = parseInBits(str)
          subPackets += nextPacket
          subConsumed += nextConsumed
          advance(nextConsumed)
        }
        (Operator(v, t, subPackets.toList), consumed)
      } else {
        val nSubPackets = toNum(advance(11)).toInt
        var packetsConsumed = 0
        val subPackets = mutable.ListBuffer.empty[Packet]
        while (packetsConsumed < nSubPackets) {
          val (nextPacket, nextConsumed) = parseInBits(str)
          subPackets += nextPacket
          packetsConsumed += 1
          advance(nextConsumed)
        }
        (Operator(v, t, subPackets.toList), consumed)
      }
    }
  }

  def parse(bits: String): Packet = {
    parseInBits(bits.flatMap(toBinary))._1
  }

  def sumVersionNumbers(packet: Packet): Int =
    packet match {
      case LiteralValue(version, _)         => version
      case Operator(version, _, subPackets) => subPackets.map(sumVersionNumbers).sum + version
    }

  def eval(packet: Packet): Long =
    packet match {
      case LiteralValue(_, value) =>
        value

      case Operator(_, 0, subPackets) =>
        subPackets.map(eval).sum

      case Operator(_, 1, subPackets) =>
        subPackets.map(eval).product

      case Operator(_, 2, subPackets) =>
        subPackets.map(eval).min

      case Operator(_, 3, subPackets) =>
        subPackets.map(eval).max

      case Operator(_, 5, subPackets) =>
        val first = eval(subPackets.head)
        val second = eval(subPackets.tail.head)
        if (first > second) 1 else 0

      case Operator(_, 6, subPackets) =>
        val first = eval(subPackets.head)
        val second = eval(subPackets.tail.head)
        if (first < second) 1 else 0

      case Operator(_, 7, subPackets) =>
        val first = eval(subPackets.head)
        val second = eval(subPackets.tail.head)
        if (first == second) 1 else 0

      case _ =>
        throw new IllegalArgumentException(s"Unknown packet $packet.")
    }

  def run(input: String): (Int, Long) = {
    val lines = input.split("\n").toList
    val packet = parse(lines.head)

    val part1 = sumVersionNumbers(packet)
    val part2 = eval(packet)

    (part1, part2)
  }
}
