package net.jcazevedo.adventofcode

class Day08 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def charsInStringLiterals(s: String) = s.size

    def replace(s: String): String = {
      val hexReplace = """([^\\](\\\\)*)\\x(\S\S)""".r.replaceAllIn(s, { m =>
        (if (m.subgroups(0) == null) "" else m.subgroups(0)) +
          (if (m.subgroups(1) == null) "" else m.subgroups(1)) +
          Integer.parseInt(m.subgroups(2), 16).toChar
      })
      if (s != hexReplace)
        replace(hexReplace)
      else
        s
    }

    def charsInMemory(s: String) = {
      val hexReplace = replace(s)
      (StringContext treatEscapes hexReplace).size - 2
    }

    def charsEscaped(s: String) = {
      s.size + s.count(_ == '"') + s.count(_ == '\\') + 2
    }

    val strings = io.Source.fromFile(filename).getLines.toList
    val tot = strings.map { s =>
      charsInStringLiterals(s) - charsInMemory(s)
    }.sum

    val tot2 = strings.map { s =>
      charsEscaped(s) - charsInStringLiterals(s)
    }.sum

    (tot, tot2)
  }
}
