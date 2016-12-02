package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day07 extends DailyChallenge[Int, Int] {
  val wires = mutable.Map[String, Wire]()

  def wireSignal(wire: String) =
    wires.get(wire) match {
      case Some(w) => w.signal
      case _ => wire.toInt
    }

  sealed trait Wire { def signal: Int }

  case class INPUT(input: String) extends Wire {
    lazy val signal: Int = wireSignal(input)
  }
  case class AND(input1: String, input2: String) extends Wire {
    lazy val signal: Int = (wireSignal(input1) & wireSignal(input2)) & 0xFFFF
  }
  case class OR(input1: String, input2: String) extends Wire {
    lazy val signal: Int = (wireSignal(input1) | wireSignal(input2)) & 0xFFFF
  }
  case class LSHIFT(input: String, count: Int) extends Wire {
    lazy val signal: Int = (wireSignal(input) << count) & 0xFFFF
  }
  case class RSHIFT(input: String, count: Int) extends Wire {
    lazy val signal: Int = (wireSignal(input) >> count) & 0xFFFF
  }
  case class NOT(input: String) extends Wire {
    lazy val signal: Int = (~wireSignal(input)) & 0xFFFF
  }

  def run(filename: String): (Int, Int) = {
    val definitions = io.Source.fromFile(filename).getLines.toList
    def load() {
      definitions.foreach { definition =>
        val defSplit = definition.split(" ")
        if (defSplit.size == 3) {
          wires(defSplit(2)) = INPUT(defSplit(0))
        } else if (defSplit(0) == "NOT") {
          val Array(_, in, _, out) = defSplit
          wires(out) = NOT(in)
        } else {
          val Array(v1, op, v2, _, out) = defSplit
          op match {
            case "AND" => wires(out) = AND(v1, v2)
            case "OR" => wires(out) = OR(v1, v2)
            case "LSHIFT" => wires(out) = LSHIFT(v1, v2.toInt)
            case "RSHIFT" => wires(out) = RSHIFT(v1, v2.toInt)
            case _ => // do nothing
          }
        }
      }
    }

    load()
    val sig = wires("a").signal
    load()
    wires("b") = INPUT(sig.toString)
    val sig2 = wires("a").signal
    (sig, sig2)
  }
}
