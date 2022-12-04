object Day13 extends DailyChallenge[Int, String] {
  case class Paper(dots: Set[(Int, Int)])

  sealed trait FoldInstruction
  case class HorizontalFold(y: Int) extends FoldInstruction
  case class VerticalFold(x: Int) extends FoldInstruction

  def applyFolds(paper: Paper, instructions: List[FoldInstruction]): Paper =
    instructions match {
      case HorizontalFold(y) :: next =>
        applyFolds(
          paper.copy(dots = paper.dots.filterNot(_._2 == y).map {
            case (dx, dy) if dy >= y => (dx, (y - (dy - y)))
            case other               => other
          }),
          next
        )
      case VerticalFold(x) :: next =>
        applyFolds(
          paper.copy(dots = paper.dots.filterNot(_._1 == x).map {
            case (dx, dy) if dx >= x => (x - (dx - x), dy)
            case other               => other
          }),
          next
        )
      case Nil => paper
    }

  def print(paper: Paper): String = {
    val minX = paper.dots.map(_._1).min
    val maxX = paper.dots.map(_._1).max
    val minY = paper.dots.map(_._2).min
    val maxY = paper.dots.map(_._2).max

    val height = maxY - minY + 1
    val length = maxX - minX + 1

    val arr = Array.fill(height)(Array.fill(length)(false))
    paper.dots.foreach { case (x, y) => arr(y - minY)(x - minX) = true }

    "\n" + arr.map(_.map(if (_) '#' else ' ').mkString).mkString("\n")
  }

  def run(input: String): (Int, String) = {
    val lines = input.split("\n").map(_.trim).toList

    val paper = Paper(
      lines
        .takeWhile(_.nonEmpty)
        .map(line => {
          val Array(x, y) = line.split(",")
          (x.toInt, y.toInt)
        })
        .toSet
    )
    val instructions = lines
      .dropWhile(_.nonEmpty)
      .tail
      .map(line => {
        val Array(l, r) = line.split("=")
        if (l.endsWith("x")) VerticalFold(r.toInt)
        else HorizontalFold(r.toInt)
      })

    val part1 = applyFolds(paper, instructions.take(1)).dots.size
    val part2 = print(applyFolds(paper, instructions))

    (part1, part2)
  }
}
