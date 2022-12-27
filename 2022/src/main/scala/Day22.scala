object Day22 extends DailyChallenge[Long, Long] {
  case class Direction(di: Int, dj: Int)

  final val Directions: Vector[Direction] = Vector(Direction(0, 1), Direction(1, 0), Direction(0, -1), Direction(-1, 0))

  case class State(i: Int, j: Int, facing: Int)

  sealed trait Instruction
  case class Forward(amount: Int) extends Instruction
  case object TurnRight extends Instruction
  case object TurnLeft extends Instruction

  def parseInstructions(instructionsStr: String): List[Instruction] =
    instructionsStr
      .foldLeft(List.empty[Instruction])({
        case (Forward(amount) :: t, ch) if ch.isDigit => Forward(amount * 10 + ch - '0') :: t
        case (l, ch) if ch.isDigit                    => Forward(ch - '0') :: l
        case (l, ch) if ch == 'R'                     => TurnRight :: l
        case (l, ch) if ch == 'L'                     => TurnLeft :: l
        case (_, ch)                                  => throw new IllegalArgumentException(s"Unexpected char $ch")
      })
      .reverse

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  final val FaceConnections: Map[(Int, Int, Int), (Int, Int, Int)] = Map(
    (0, 1, 2) -> (2, 0, 0),
    (0, 1, 3) -> (3, 0, 0),
    (0, 2, 0) -> (2, 1, 2),
    (0, 2, 1) -> (1, 1, 2),
    (0, 2, 3) -> (3, 0, 3),
    (1, 1, 0) -> (0, 2, 3),
    (1, 1, 2) -> (2, 0, 1),
    (2, 0, 2) -> (0, 1, 0),
    (2, 0, 3) -> (1, 1, 0),
    (2, 1, 0) -> (0, 2, 2),
    (2, 1, 1) -> (3, 0, 2),
    (3, 0, 0) -> (2, 1, 3),
    (3, 0, 1) -> (0, 2, 1),
    (3, 0, 2) -> (0, 1, 1)
  )

  def applyInstructions(grid: Vector[String], instructions: List[Instruction], cubed: Boolean): State = {
    val height = grid.length
    val width = grid.map(_.length).max
    val faceLength = gcd(height, width)

    def rotateRight(ij: (Int, Int)): (Int, Int) =
      (ij._2, faceLength - ij._1 - 1)

    def rotate(ij: (Int, Int), currentFacing: Int, targetFacing: Int): (Int, Int) =
      if (currentFacing == targetFacing) ij
      else rotate(rotateRight(ij), (currentFacing + 1) % Directions.length, targetFacing)

    def outsideBoard(s: State): Boolean =
      s.i < 0 || s.i >= grid.length || s.j < 0 || s.j >= grid(s.i).length || grid(s.i)(s.j) == ' '

    def inWall(s: State): Boolean =
      !outsideBoard(s) && grid(s.i)(s.j) == '#'

    def forward(s: State, prev: Option[State] = None): State = {
      val ni = s.i + Directions(s.facing).di
      val nj = s.j + Directions(s.facing).dj
      val tmp = State((ni + height) % height, (nj + width) % width, s.facing)
      if (outsideBoard(tmp))
        if (cubed) {
          val (nextFaceI, nextFaceJ, nextFacing) = FaceConnections((s.i / faceLength, s.j / faceLength, s.facing))
          val (afterRotI, afterRotJ) = rotate((s.i % faceLength, s.j % faceLength), s.facing, nextFacing)
          val (afterReposI, afterReposJ) =
            if (nextFacing == 0) (nextFaceI * faceLength + afterRotI, nextFaceJ * faceLength - 1)
            else if (nextFacing == 1) (nextFaceI * faceLength - 1, nextFaceJ * faceLength + afterRotJ)
            else if (nextFacing == 2) (nextFaceI * faceLength + afterRotI, (nextFaceJ + 1) * faceLength)
            else ((nextFaceI + 1) * faceLength, nextFaceJ * faceLength + afterRotJ)
          val reposState = State(i = afterReposI, j = afterReposJ, facing = nextFacing)
          forward(reposState, prev.orElse(Some(s)))
        } else forward(tmp, prev.orElse(Some(s)))
      else if (inWall(tmp)) prev.getOrElse(s)
      else tmp
    }

    def move(s: State, amount: Int): State =
      if (amount == 0) s
      else move(forward(s), amount - 1)

    def aux(s: State, i: List[Instruction]): State =
      i match {
        case Forward(amount) :: next =>
          aux(move(s, amount), next)

        case TurnRight :: next =>
          aux(s.copy(facing = (s.facing + 1) % Directions.length), next)

        case TurnLeft :: next =>
          aux(s.copy(facing = (s.facing + Directions.length - 1) % Directions.length), next)

        case Nil =>
          s
      }

    val startingRow = grid.zipWithIndex.find({ case (line, i) => line.contains('.') }).get._2
    val startingCol = grid(startingRow).indexOf('.')
    val startingDir = 0

    aux(State(i = startingRow, j = startingCol, facing = startingDir), instructions)
  }

  def password(state: State): Int =
    1000 * (state.i + 1) + 4 * (state.j + 1) + state.facing

  def run(input: String): (Long, Long) = {
    val lines = input.split("\n").toVector
    val blankLine = lines.indexOf("")
    val (grid, instructionStr) = lines.splitAt(blankLine)
    val instructions = parseInstructions(instructionStr.tail.head)

    val part1 = password(applyInstructions(grid, instructions, cubed = false))
    val part2 = password(applyInstructions(grid, instructions, cubed = true))

    (part1, part2)
  }
}
