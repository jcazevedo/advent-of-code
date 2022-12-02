object Day02 extends DailyChallenge[Int, Int] {
  sealed trait Shape {
    def score: Int
    def defeats(shape: Shape): Boolean
  }

  object Shape {
    final val shapes: List[Shape] = List(Rock, Paper, Scissors)
  }

  case object Rock extends Shape {
    val score: Int = 1
    def defeats(shape: Shape): Boolean = shape == Scissors
  }

  case object Paper extends Shape {
    val score: Int = 2
    def defeats(shape: Shape): Boolean = shape == Rock
  }

  case object Scissors extends Shape {
    val score: Int = 3
    def defeats(shape: Shape): Boolean = shape == Paper
  }

  def roundScore(theirShape: Shape, myShape: Shape): Int =
    if (myShape.defeats(theirShape)) 6 + myShape.score
    else if (theirShape.defeats(myShape)) myShape.score
    else 3 + myShape.score

  def totalScore(rounds: List[(Shape, Shape)]): Int =
    rounds.map({ case (theirs, mine) => roundScore(theirs, mine) }).sum

  def run(input: String): (Int, Int) = {
    val lineCodes = input
      .split("\n")
      .map { line =>
        val Array(x, y) = line.split(" ")
        (x, y)
      }
      .toList

    val roundsPart1 = lineCodes.map { case ((opponentCode, myCode)) =>
      val opponentShape = opponentCode match {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      }
      val myShape = myCode match {
        case "X" => Rock
        case "Y" => Paper
        case "Z" => Scissors
      }
      (opponentShape, myShape)
    }

    val roundsPart2 = lineCodes.map { case ((opponentCode, myCode)) =>
      val opponentShape = opponentCode match {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      }
      val myShape = myCode match {
        case "X" => Shape.shapes.find(opponentShape.defeats).get
        case "Y" => Shape.shapes.find(mine => !mine.defeats(opponentShape) && !opponentShape.defeats(mine)).get
        case "Z" => Shape.shapes.find(_.defeats(opponentShape)).get
      }
      (opponentShape, myShape)
    }

    val part1 = totalScore(roundsPart1)
    val part2 = totalScore(roundsPart2)

    (part1, part2)
  }
}
