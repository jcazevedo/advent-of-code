import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable

object Day21 extends DailyChallenge[String, String] {
  sealed trait Op
  case class Add(v1: Op, v2: Op) extends Op {
    override def toString = s"($v1 + $v2)"
  }
  case class Mul(v1: Op, v2: Op) extends Op {
    override def toString = s"($v1 * $v2)"
  }
  case class Sub(v1: Op, v2: Op) extends Op {
    override def toString = s"($v1 - $v2)"
  }
  case class Div(v1: Op, v2: Op) extends Op {
    override def toString = s"($v1 / $v2)"
  }
  case class Eq(v1: Op, v2: Op) extends Op {
    override def toString = s"($v1 = $v2)"
  }
  case class Val(value: Long) extends Op {
    override def toString = value.toString
  }
  case class Var(name: String) extends Op {
    override def toString = name
  }
  case class Expression(name: Var, op: Op)

  object Parser extends RegexParsers {
    def value: Parser[Val] = """\d+""".r ^^ (v => Val(v.toInt))
    def variable: Parser[Var] = """[a-z]+""".r ^^ (v => Var(v))

    def operator: Parser[String] = "+" | "*" | "-" | "/"

    def arithmetic: Parser[Op] = variable ~ operator ~ variable ^^ {
      case v1 ~ "+" ~ v2 => Add(v1, v2)
      case v1 ~ "*" ~ v2 => Mul(v1, v2)
      case v1 ~ "-" ~ v2 => Sub(v1, v2)
      case v1 ~ "/" ~ v2 => Div(v1, v2)
      case _ ~ op ~ _    => throw new IllegalArgumentException(s"Unknown operator $op")
    }

    def op: Parser[Op] = arithmetic | value

    def expr: Parser[Expression] = (variable <~ ": ") ~ op ^^ { case v ~ op => Expression(v, op) }

    def parse(str: String): Expression = parse(expr, str).get
  }

  def eval(toEval: Op, prelude: List[Expression] = List.empty): Op = {
    val st = prelude.map(exp => exp.name.name -> exp.op).toMap

    def loop(curr: Op): Op =
      curr match {
        case Add(v1, v2) =>
          (loop(v1), loop(v2)) match {
            case (Val(a), Val(b)) => Val(a + b)
            case (a, b)           => Add(a, b)
          }
        case Mul(v1, v2) =>
          (loop(v1), loop(v2)) match {
            case (Val(a), Val(b)) => Val(a * b)
            case (a, b)           => Mul(a, b)
          }
        case Sub(v1, v2) =>
          (loop(v1), loop(v2)) match {
            case (Val(a), Val(b)) => Val(a - b)
            case (a, b)           => Sub(a, b)
          }
        case Div(v1, v2) =>
          (loop(v1), loop(v2)) match {
            case (Val(a), Val(b)) => Val(a / b)
            case (a, b)           => Div(a, b)
          }
        case Eq(v1, v2) =>
          (loop(v1), loop(v2)) match {
            case (Val(a), Val(b)) => Val(if (a == b) 1 else 0)
            case (a, b)           => Eq(a, b)
          }
        case v: Val        => v
        case v @ Var(name) => if (st.contains(name)) loop(st(name)) else v
      }

    loop(toEval)
  }

  def reduce(op: Op): Op = {
    val visited = mutable.Set.empty[Op]

    def aux(op: Op): Op =
      if (visited(op)) op
      else {
        visited.add(op)
        op match {
          case Eq(Val(a), Add(Val(b), c)) => aux(Eq(eval(Sub(Val(a), Val(b))), c))
          case Eq(Val(a), Add(b, Val(c))) => aux(Eq(eval(Sub(Val(a), Val(c))), b))
          case Eq(Val(a), Mul(Val(b), c)) => aux(Eq(eval(Div(Val(a), Val(b))), c))
          case Eq(Val(a), Mul(b, Val(c))) => aux(Eq(eval(Div(Val(a), Val(c))), b))
          case Eq(Val(a), Sub(Val(b), c)) => aux(Eq(eval(Sub(Val(0), Sub(Val(a), Val(b)))), c))
          case Eq(Val(a), Sub(b, Val(c))) => aux(Eq(eval(Add(Val(a), Val(c))), b))
          case Eq(Val(a), Div(Val(b), c)) => aux(Eq(eval(Div(Val(b), Val(a))), c))
          case Eq(Val(a), Div(b, Val(c))) => aux(Eq(eval(Mul(Val(a), Val(c))), b))
          case Eq(a, Val(b))              => aux(Eq(Val(b), a))
          case other                      => other
        }
      }

    aux(op)
  }

  def run(input: String): (String, String) = {
    val expressions = input.split("\n").map(_.trim).map(line => Parser.parse(line)).toList

    val part1 = eval(Var("root"), expressions)

    val part2 = reduce(
      eval(
        Var("root"),
        expressions
          .filterNot(_.name.name == "humn")
          .map({
            case Expression(Var("root"), Add(a, b)) => Expression(Var("root"), Eq(a, b))
            case Expression(Var("root"), Mul(a, b)) => Expression(Var("root"), Eq(a, b))
            case Expression(Var("root"), Sub(a, b)) => Expression(Var("root"), Eq(a, b))
            case Expression(Var("root"), Div(a, b)) => Expression(Var("root"), Eq(a, b))
            case other                              => other
          })
      )
    )

    (part1.toString, part2.toString)
  }
}
