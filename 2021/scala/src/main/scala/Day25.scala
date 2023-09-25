import scala.collection.mutable

object Day25 extends DailyChallenge[Int, Unit] {
  def next(cucumbers: Vector[String]): Vector[String] = {
    val grid = cucumbers.toArray.map(_.toArray)

    def move(ch: Char, direction: (Int, Int)) = {
      val toMove = for {
        i <- 0 until grid.length
        j <- 0 until grid(i).length
        if grid(i)(j) == ch
        ni = (i + direction._1) % grid.length
        nj = (j + direction._2) % grid(i).length
        if grid(ni)(nj) == '.'
      } yield ((i, j), (ni, nj))

      toMove.foreach { case ((fromI, fromJ), (toI, toJ)) =>
        grid(fromI)(fromJ) = '.'
        grid(toI)(toJ) = ch
      }
    }

    move('>', (0, 1))
    move('v', (1, 0))

    grid.toVector.map(_.mkString)
  }

  def run(input: String): (Int, Unit) = {
    val cucumbers = input.split("\n").toVector
    val part1 = Iterator.iterate(cucumbers)(next).sliding(2).takeWhile(p => p(0) != p(1)).length + 1
    (part1, ())
  }
}
