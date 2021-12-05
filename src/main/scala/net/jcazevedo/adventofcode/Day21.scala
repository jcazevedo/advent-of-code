package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day21 extends DailyChallenge[Int, Int] {
  def flip(p: Vector[String]): Vector[String] = {
    val L = p.length
    val res = (0 until L).map(_ => "." * L).toVector
    (0 until L).foldLeft(res) {
      case (res, i) =>
        (0 until L).foldLeft(res) {
          case (res, j) =>
            res.updated(i, res(i).updated(j, p(i)(L - j - 1)))
        }
    }
  }

  def rotate(p: Vector[String]): Vector[String] = {
    val L = p.length
    val res = (0 until L).map(_ => "." * L).toVector
    (0 until L).foldLeft(res) {
      case (res, i) =>
        (0 until L).foldLeft(res) {
          case (res, j) =>
            res.updated(i, res(i).updated(j, p(j)(L - i - 1)))
        }
    }
  }

  case class Pattern(input: Vector[String], output: Vector[String]) {
    def doesMatch(p: Vector[String]): Boolean = {
      val pos = Iterator.iterate(p)(rotate(_)).take(4) ++ Iterator.iterate(flip(p))(rotate(_)).take(4)
      pos.exists(_ == input)
    }
  }

  def split(v: Vector[String]): Vector[Vector[Vector[String]]] = {
    val L = v.length
    val d = if (L % 2 == 0) 2 else 3
    val res = (0 until L / d).map(_ => (0 until L / d).map(_ => (0 until d).map(_ => "." * d).toVector).toVector).toVector
    (0 until L).foldLeft(res) {
      case (res, i) =>
        (0 until L).foldLeft(res) {
          case (res, j) =>
            val si = i / d
            val sj = j / d
            val isi = (i - si * d) % d
            val isj = (j - sj * d) % d
            res.updated(si, res(si).updated(sj, res(si)(sj).updated(isi, res(si)(sj)(isi).updated(isj, v(i)(j)))))
        }
    }
  }

  def join(v: Vector[Vector[Vector[String]]]): Vector[String] = {
    val L = v.length
    val IL = v.head.head.length
    val FL = L * IL
    val res = (0 until FL).map(_ => "." * FL).toVector
    (0 until FL).foldLeft(res) {
      case (res, i) =>
        (0 until FL).foldLeft(res) {
          case (res, j) =>
            val si = i / IL
            val sj = j / IL
            val isi = (i - si * IL) % IL
            val isj = (j - sj * IL) % IL
            res.updated(i, res(i).updated(j, v(si)(sj)(isi)(isj)))
        }
    }
  }

  def run(filename: String): (Int, Int) = {
    val lines = io.Source.fromFile(filename).getLines().toList

    val patterns = lines.map { l =>
      val Array(is, os) = l.split(" => ")
      Pattern(is.split("/").toVector, os.split("/").toVector)
    }

    val start = Vector(".#.", "..#", "###")

    val cache = mutable.Map[Vector[String], Vector[String]]()

    def getOutput(square: Vector[String]): Vector[String] = {
      cache.getOrElseUpdate(square, patterns.find(_.doesMatch(square)) match {
        case Some(p) => p.output
        case None =>
          println(s"No pattern found that matches $square")
          return Vector.empty
      })
    }

    def run(grid: Vector[String], iterations: Int): Vector[String] =
      iterations match {
        case 0 => grid
        case n => run(join(split(grid).map(_.map(getOutput))), n - 1)
      }

    val res1 = run(start, 5).map(_.count(_ == '#')).sum
    val res2 = run(start, 18).map(_.count(_ == '#')).sum

    (res1, res2)
  }
}
