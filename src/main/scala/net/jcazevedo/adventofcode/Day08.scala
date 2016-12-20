package net.jcazevedo.adventofcode

class Day08 extends DailyChallenge[Int, String] {
  sealed trait Instruction
  case class Rect(w: Int, h: Int) extends Instruction
  case class RotateRow(r: Int, l: Int) extends Instruction
  case class RotateCol(c: Int, l: Int) extends Instruction

  def rotateRight(s: String): String =
    s.last + s.dropRight(1)

  def apply(rectangle: Seq[String], instruction: Instruction): Seq[String] = {
    instruction match {
      case Rect(w, h) =>
        rectangle.zipWithIndex.map {
          case (s, i) =>
            if (i < h)
              s.zipWithIndex.map {
                case (ch, j) =>
                  if (j < w) '#' else ch
              }.mkString
            else
              s
        }
      case RotateRow(r, l) =>
        rectangle.zipWithIndex.map {
          case (s, i) =>
            if (i == r)
              (0 until l).foldLeft(s) {
                case (s, _) =>
                  rotateRight(s)
              }
            else
              s
        }
      case RotateCol(c, l) =>
        val column = (0 until l).foldLeft(rectangle.map(_(c)).mkString) {
          case (s, _) =>
            rotateRight(s)
        }
        rectangle.zipWithIndex.map {
          case (s, i) =>
            s.zipWithIndex.map {
              case (ch, j) =>
                if (j == c) {
                  column(i)
                } else
                  ch
            }.mkString
        }
    }
  }

  def run(filename: String): (Int, String) = {
    val instructionsStr = io.Source.fromFile(filename).getLines.toList
    val instructions = instructionsStr.map { s =>
      if (s.startsWith("rect")) {
        val ss = s.split("x")
        Rect(ss(0).dropWhile(!_.isDigit).toInt, ss(1).toInt)
      } else if (s.startsWith("rotate row")) {
        val ss = s.split(" by ")
        RotateRow(ss(0).dropWhile(!_.isDigit).toInt, ss(1).toInt)
      } else {
        val ss = s.split(" by ")
        RotateCol(ss(0).dropWhile(!_.isDigit).toInt, ss(1).toInt)
      }
    }

    val rectangle: Seq[String] = (0 until 6).map { _ =>
      "." * 50
    }

    val res = instructions.foldLeft(rectangle) {
      case (rect, inst) =>
        apply(rect, inst)
    }

    (res.map(_.count(_ == '#')).sum, "\n" + res.mkString("\n"))
  }
}
