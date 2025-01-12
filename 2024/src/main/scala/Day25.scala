object Day25 extends DailyChallenge[Int, Int] {
  final val MaxHeight = 5

  case class Lock(heights: Vector[Int])
  case class Key(heights: Vector[Int])

  def fits(lock: Lock, key: Key): Boolean =
    lock.heights.zip(key.heights).forall((h1, h2) => h1 + h2 <= MaxHeight)

  def fromSchematic(schematic: Vector[String]): Either[Lock, Key] = {
    val W = schematic(0).length
    if (schematic(0)(0) == '#')
      Left(Lock((0 until W).map(w => (0 to MaxHeight).find(i => schematic(i + 1)(w) == '.').get).toVector))
    else
      Right(Key((0 until W).map(w => (0 to MaxHeight).find(i => schematic(MaxHeight - i)(w) == '.').get).toVector))
  }

  def run(input: String): (Int, Int) = {
    val schematics = input
      .split("\n")
      .foldLeft(Vector(Vector.empty[String]))((curr, line) =>
        if (line.isEmpty) curr :+ Vector.empty[String]
        else curr.updated(curr.size - 1, curr(curr.size - 1) :+ line)
      )
    val (locks, keys) =
      schematics.foldLeft((Vector.empty[Lock], Vector.empty[Key]))({ case ((locks, keys), schematic) =>
        fromSchematic(schematic) match {
          case Left(lock) => (locks :+ lock, keys)
          case Right(key) => (locks, keys :+ key)
        }
      })

    val part1 = (for {
      lock <- locks
      key <- keys
      if fits(lock, key)
    } yield (lock, key)).size

    (part1, -1)
  }
}
