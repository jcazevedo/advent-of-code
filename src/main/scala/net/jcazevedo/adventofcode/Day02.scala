package net.jcazevedo.adventofcode

class Day02 extends DailyChallenge[String, String] {
  val basicKeypad = Array(
    Array('1', '2', '3'),
    Array('4', '5', '6'),
    Array('7', '8', '9'))

  val complexKeypad = Array(
    Array(' ', ' ', '1', ' ', ' '),
    Array(' ', '2', '3', '4', ' '),
    Array('5', '6', '7', '8', '9'),
    Array(' ', 'A', 'B', 'C', ' '),
    Array(' ', ' ', 'D', ' ', ' '))

  val dirs = Map(
    'U' -> ((-1, 0)),
    'D' -> ((1, 0)),
    'L' -> ((0, -1)),
    'R' -> ((0, 1)))

  def nextPosition(keypad: Array[Array[Char]])(currentPos: (Int, Int), dir: Char): (Int, Int) = {
    val inc = dirs(dir)
    val nextPos = (currentPos._1 + inc._1, currentPos._2 + inc._2)
    if (nextPos._1 < 0 ||
      nextPos._1 >= keypad.size ||
      nextPos._2 < 0 ||
      nextPos._2 >= keypad(nextPos._1).size ||
      keypad(nextPos._1)(nextPos._2) == ' ')
      currentPos
    else
      nextPos
  }

  def getCode(lines: List[String], keypad: Array[Array[Char]], start: (Int, Int)) = {
    val (code, _) = lines.foldLeft(("", start)) {
      case ((code, pos), line) =>
        val nextPos = line.foldLeft(pos) { case x => (nextPosition(keypad) _).tupled(x) }
        (code + keypad(nextPos._1)(nextPos._2), nextPos)
    }
    code
  }

  def run(filename: String) = {
    val lines = io.Source.fromFile(filename).getLines.toList
    val simpleCode = getCode(lines, basicKeypad, (1, 1))
    val complexCode = getCode(lines, complexKeypad, (2, 0))
    (simpleCode, complexCode)
  }
}
