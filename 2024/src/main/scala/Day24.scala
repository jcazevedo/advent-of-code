import scala.collection.mutable
import scala.annotation.tailrec

object Day24 extends DailyChallenge[Long, String] {
  sealed trait Wire
  case class Input(value: Boolean) extends Wire
  case class And(wire1: String, wire2: String) extends Wire
  case class Or(wire1: String, wire2: String) extends Wire
  case class Xor(wire1: String, wire2: String) extends Wire

  def simulateOnce(gates: Map[String, Wire]): Map[String, Wire] = {
    def input(wire: String): Option[Boolean] = gates
      .get(wire)
      .flatMap({
        case Input(value) => Some(value)
        case _            => None
      })

    gates.map({
      case (wire, Input(value)) =>
        wire -> Input(value)
      case (wire, op @ And(wire1, wire2)) =>
        wire -> (for {
          v1 <- input(wire1)
          v2 <- input(wire2)
        } yield Input(v1 && v2)).getOrElse(op)
      case (wire, op @ Or(wire1, wire2)) =>
        wire -> (for {
          v1 <- input(wire1)
          v2 <- input(wire2)
        } yield Input(v1 || v2)).getOrElse(op)
      case (wire, op @ Xor(wire1, wire2)) =>
        wire -> (for {
          v1 <- input(wire1)
          v2 <- input(wire2)
        } yield Input(v1 ^ v2)).getOrElse(op)
    })
  }

  @tailrec
  def simulate(gates: Map[String, Wire]): Map[String, Wire] = {
    val next = simulateOnce(gates)
    if (next == gates) gates
    else simulate(next)
  }

  def outputNumber(gates: Map[String, Wire], prefix: String): Long =
    gates
      .collect({ case (k, Input(v)) if k.startsWith(prefix) => k -> v })
      .toList
      .sortBy(_._1)
      .map(_._2)
      .reverse
      .foldLeft(0L)((curr, next) => curr * 2 + (if (next) 1 else 0))

  def swap(gates: Map[String, Wire], wire1: String, wire2: String): Map[String, Wire] =
    gates.updated(wire1, gates(wire2)).updated(wire2, gates(wire1))

  def dependencies(gates: Map[String, Wire], wire: String): Set[String] =
    Set(wire) ++ (gates(wire) match {
      case Input(value)      => Set.empty
      case And(wire1, wire2) => dependencies(gates, wire1) ++ dependencies(gates, wire2)
      case Or(wire1, wire2)  => dependencies(gates, wire1) ++ dependencies(gates, wire2)
      case Xor(wire1, wire2) => dependencies(gates, wire1) ++ dependencies(gates, wire2)
    })

  def bitWire(i: Int, prefix: String): String =
    f"$prefix$i%02d"

  def isOr(gates: Map[String, Wire], wire: String, fw1: String => Boolean, fw2: String => Boolean): Boolean =
    gates(wire) match {
      case Or(wire1, wire2) => (fw1(wire1) && fw2(wire2)) || (fw2(wire1) && fw1(wire2))
      case _                => false
    }

  def isAnd(gates: Map[String, Wire], wire: String, fw1: String => Boolean, fw2: String => Boolean): Boolean =
    gates(wire) match {
      case And(wire1, wire2) => (fw1(wire1) && fw2(wire2)) || (fw2(wire1) && fw1(wire2))
      case _                 => false
    }

  def isXor(gates: Map[String, Wire], wire: String, fw1: String => Boolean, fw2: String => Boolean): Boolean =
    gates(wire) match {
      case Xor(wire1, wire2) => (fw1(wire1) && fw2(wire2)) || (fw2(wire1) && fw1(wire2))
      case _                 => false
    }

  def isOverflow(gates: Map[String, Wire], bit: Int, wire: String): Boolean = {
    val x = bitWire(bit, "x")
    val y = bitWire(bit, "y")
    if (bit == 0) isAnd(gates, wire, _ == x, _ == y)
    else
      isOr(
        gates,
        wire,
        isAnd(gates, _, isXor(gates, _, _ == x, _ == y), isOverflow(gates, bit - 1, _)),
        isAnd(gates, _, _ == x, _ == y)
      )
  }

  def isAdd(gates: Map[String, Wire], wire: String): Boolean =
    if (!wire.startsWith("z")) false
    else {
      val bit = wire.drop(1).toInt
      val x = bitWire(bit, "x")
      val y = bitWire(bit, "y")
      if (bit == 0) isXor(gates, wire, _ == x, _ == y)
      else isXor(gates, wire, isXor(gates, _, _ == x, _ == y), isOverflow(gates, bit - 1, _))
    }

  def swapWires(gates: Map[String, Wire]): Set[(String, String)] = {
    val wiresToSwap = gates.collect({ case (w, wire) if !wire.isInstanceOf[Input] => w }).toSet
    val zbits = gates.count(_._1.startsWith("z"))

    def go(
        curr: Map[String, Wire],
        candidates: Set[String],
        currSwaps: Set[(String, String)],
        bit: Int
    ): Option[Set[(String, String)]] = {
      if (bit >= zbits - 1) Some(currSwaps)
      else if (currSwaps.size > 4) None
      else {
        val currWire = bitWire(bit, "z")
        val currWireDeps = dependencies(curr, currWire)
        if (isAdd(curr, bitWire(bit, "z"))) go(curr, candidates -- currWireDeps, currSwaps, bit + 1)
        else {
          (for {
            w1 <- currWireDeps
            w2 <- candidates - w1
            nextGates = swap(curr, w1, w2)
            if isAdd(nextGates, currWire)
            next = go(nextGates, candidates -- dependencies(nextGates, currWire), currSwaps + ((w1, w2)), bit + 1)
            if next.nonEmpty
          } yield next.get).headOption
        }
      }
    }

    go(gates, wiresToSwap, Set.empty, 0).get
  }

  def run(input: String): (Long, String) = {
    val gates = {
      val tmp = mutable.Map.empty[String, Wire]
      input
        .split("\n")
        .foreach(line => {
          if (line.nonEmpty) {
            val splits = line.split(" ")
            if (splits.size == 2)
              tmp.update(splits(0).dropRight(1), Input(splits(1).toInt == 1))
            else
              tmp.update(
                splits(4),
                splits(1) match {
                  case "AND" => And(splits(0), splits(2))
                  case "OR"  => Or(splits(0), splits(2))
                  case "XOR" => Xor(splits(0), splits(2))
                }
              )
          }
        })
      tmp.toMap
    }

    val part1 = outputNumber(simulate(gates), "z")
    val part2 = swapWires(gates).flatMap((a, b) => Set(a, b)).toList.sorted.mkString(",")

    (part1, part2)
  }
}
