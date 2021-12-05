package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day10 extends DailyChallenge[Int, Int] {
  val targets = mutable.Set[Target]()

  trait Target {
    def id: Int
    def addValue(v: Int): Unit
    def enable(): Unit
  }

  class Output(val id: Int) extends Target {
    var value: Option[Int] = None
    def addValue(v: Int): Unit = { value = Some(v) }
    def enable(): Unit = {}
  }

  class Bot(val id: Int) extends Target {
    var values: List[Int] = List()
    var highTo: Option[Target] = None
    var lowTo: Option[Target] = None
    var enabled: Boolean = false
    def addValue(v: Int): Unit = { values = (v :: values).sorted }
    def high = values.last
    def low = values.head
    def valid: Boolean = values.length == 2 && highTo.isDefined && lowTo.isDefined
    def setHighTo(t: Target): Unit = { highTo = Some(t) }
    def setLowTo(t: Target): Unit = { lowTo = Some(t) }
    def enable() = {
      if (valid && !enabled) {
        highTo.get.addValue(high)
        lowTo.get.addValue(low)
        highTo.get.enable()
        lowTo.get.enable()
        enabled = true
      }
    }
  }

  def getTarget(typ: String, idx: Int): Target = {
    val curr = targets.find {
      case t: Bot if typ == "bot" && t.id == idx => true
      case t: Output if typ == "output" && t.id == idx => true
      case _ => false
    }
    curr match {
      case Some(t) => t
      case None =>
        val next = if (typ == "bot") new Bot(idx) else new Output(idx)
        targets.add(next)
        next
    }
  }

  def run(filename: String): (Int, Int) = {
    val instructions = io.Source.fromFile(filename).getLines.toList
    instructions.foreach { instruction =>
      val splits = instruction.trim.split("\\s+")
      if (splits(0) == "value") {
        val value = splits(1).toInt
        val target = getTarget(splits(4), splits(5).toInt)
        target.addValue(value)
        target.enable()
      } else {
        val orig = getTarget("bot", splits(1).toInt).asInstanceOf[Bot]
        val targetLow = getTarget(splits(5), splits(6).toInt)
        val targetHigh = getTarget(splits(10), splits(11).toInt)
        orig.setLowTo(targetLow)
        orig.setHighTo(targetHigh)
        orig.enable()
      }
    }
    val targetBot = targets.find {
      case t: Bot if t.valid && t.high == 61 && t.low == 17 => true
      case _ => false
    }
    val v1 = getTarget("output", 0).asInstanceOf[Output].value.get
    val v2 = getTarget("output", 1).asInstanceOf[Output].value.get
    val v3 = getTarget("output", 2).asInstanceOf[Output].value.get
    (targetBot.get.id, v1 * v2 * v3)
  }
}
