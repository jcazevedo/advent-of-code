package adventofcode

import scala.util.Try

class Day04 extends DailyChallenge[Int, Int] {
  case class Passport(fields: Map[String, String])

  val fields: Map[String, String => Boolean] = Map(
    "byr" -> { s => Try(s.toInt).fold(_ => false, v => v >= 1920 && v <= 2002) },
    "iyr" -> { s => Try(s.toInt).fold(_ => false, v => v >= 2010 && v <= 2020) },
    "eyr" -> { s => Try(s.toInt).fold(_ => false, v => v >= 2020 && v <= 2030) },
    "hgt" -> { s =>
      (s.endsWith("cm") && Try(s.stripSuffix("cm").toInt).fold(_ => false, v => v >= 150 && v <= 193)) ||
      (s.endsWith("in") && Try(s.stripSuffix("in").toInt).fold(_ => false, v => v >= 59 && v <= 76))
    },
    "hcl" -> { s =>
      s.head == '#' && s.tail.length == 6 && s.tail.forall(ch => (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f'))
    },
    "ecl" -> Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").apply,
    "pid" -> { s => s.length == 9 && Try(s.toInt).isSuccess }
  )

  def mkPassport(str: String): Passport = {
    Passport(
      str
        .split(" ")
        .map { field =>
          val Array(k, v) = field.split(":")
          k -> v
        }
        .toMap
    )
  }

  def isValid1(passport: Passport): Boolean =
    (fields.keySet -- passport.fields.keySet).isEmpty

  def isValid2(passport: Passport): Boolean =
    isValid1(passport) && passport.fields.forall { case (k, v) => fields.get(k).forall(_(v)) }

  def run(input: String): (Int, Int) = {
    val passports = input
      .split("\n")
      .foldRight(List("")) { case (curr, acc) =>
        curr match {
          case ""    => "" :: acc
          case other => (other + " " + acc.head) :: acc.tail
        }
      }
      .map(mkPassport)

    (passports.count(isValid1), passports.count(isValid2))
  }
}
