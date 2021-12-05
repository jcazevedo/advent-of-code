package adventofcode

import scala.util.parsing.combinator._

class Day18 extends DailyChallenge[Long, Long] {
  sealed trait Expr {
    def eval: Long
  }
  case class Num(v: Long) extends Expr {
    def eval: Long = v
  }
  case class Sum(e1: Expr, e2: Expr) extends Expr {
    def eval: Long = e1.eval + e2.eval
  }
  case class Mul(e1: Expr, e2: Expr) extends Expr {
    def eval: Long = e1.eval * e2.eval
  }

  trait BaseParsers extends RegexParsers {
    override def skipWhitespace: Boolean = true
    def num: Parser[Num] = """(0|[1-9]\d*)""".r ^^ { v => Num(v.toLong) }
  }

  class Parser1 extends BaseParsers {
    def term: Parser[Expr] = num | "(" ~> expr <~ ")"
    def sum: Parser[Expr => Expr] = "+" ~> term ^^ { e => Sum(_, e) }
    def mul: Parser[Expr => Expr] = "*" ~> term ^^ { e => Mul(_, e) }
    def expr: Parser[Expr] = term ~ rep(sum | mul) ^^ { case t ~ ops =>
      ops.foldLeft[Expr](t) { case (acc, op) => op(acc) }
    }
  }

  object Parser1 extends Parser1 {
    def parse(str: String): Expr = parse(expr, str).get
  }

  def part1(expr: List[String]): Long = expr.map(Parser1.parse).map(_.eval).sum

  class Parser2 extends BaseParsers {
    def sum: Parser[Expr => Expr] = "+" ~> term ^^ { e => Sum(_, e) }
    def mul: Parser[Expr => Expr] = "*" ~> factor ^^ { e => Mul(_, e) }
    def term: Parser[Expr] = num | "(" ~> expr <~ ")"
    def factor: Parser[Expr] = term ~ rep(sum) ^^ { case t ~ ops =>
      ops.foldLeft[Expr](t) { case (acc, op) => op(acc) }
    }
    def expr: Parser[Expr] = factor ~ rep(mul) ^^ { case t ~ ops =>
      ops.foldLeft[Expr](t) { case (acc, op) => op(acc) }
    }
  }

  object Parser2 extends Parser2 {
    def parse(str: String): Expr = parse(expr, str).get
  }

  def part2(expr: List[String]): Long = expr.map(Parser2.parse).map(_.eval).sum

  def run(input: String): (Long, Long) = {
    val expr = input.split("\n").toList
    (part1(expr), part2(expr))
  }
}
