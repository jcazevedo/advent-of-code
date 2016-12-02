package net.jcazevedo.adventofcode

import spray.json._

class Day12 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    val input = io.Source.fromFile(filename).getLines.mkString

    def getCount(js: JsValue): Int = {
      js match {
        case JsObject(fields) => fields.map(v => getCount(v._2)).sum
        case JsNumber(value) => value.toInt
        case JsArray(elements) => elements.map(getCount).sum
        case _ => 0
      }
    }

    def getFilteredCount(js: JsValue): Int = {
      js match {
        case JsObject(fields) =>
          if (fields.map(_._2).toList.contains(JsString("red"))) 0
          else fields.map(v => getFilteredCount(v._2)).sum
        case JsNumber(value) => value.toInt
        case JsArray(elements) => elements.map(getFilteredCount).sum
        case _ => 0
      }
    }

    val json = input.parseJson

    (getCount(json), getFilteredCount(json))
  }
}
