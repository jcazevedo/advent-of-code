import scala.collection.mutable

object Day22 extends DailyChallenge[Long, Long] {
  case class Range(min: Long, max: Long) {
    def intersection(other: Range): Option[Range] =
      Option.when(max >= other.min && min <= other.max)(Range(math.max(min, other.min), math.min(max, other.max)))

    def length: Long =
      max - min + 1

    def inside(other: Range): Boolean =
      min >= other.min && max <= other.max
  }

  case class Cuboid(xs: Range, ys: Range, zs: Range) {
    def intersection(other: Cuboid): Option[Cuboid] =
      for {
        newXs <- xs.intersection(other.xs)
        newYs <- ys.intersection(other.ys)
        newZs <- zs.intersection(other.zs)
      } yield Cuboid(newXs, newYs, newZs)

    def volume: Long =
      xs.length * ys.length * zs.length

    def removeHole(hole: Cuboid): Set[Cuboid] = {
      val ans = mutable.ListBuffer.empty[Cuboid]
      if (xs.min != hole.xs.min)
        ans += this.copy(xs = Range(xs.min, hole.xs.min - 1))
      if (xs.max != hole.xs.max)
        ans += this.copy(xs = Range(hole.xs.max + 1, xs.max))
      if (ys.min != hole.ys.min)
        ans += this.copy(xs = hole.xs, ys = Range(ys.min, hole.ys.min - 1))
      if (ys.max != hole.ys.max)
        ans += this.copy(xs = hole.xs, ys = Range(hole.ys.max + 1, ys.max))
      if (zs.min != hole.zs.min)
        ans += this.copy(xs = hole.xs, ys = hole.ys, zs = Range(zs.min, hole.zs.min - 1))
      if (zs.max != hole.zs.max)
        ans += this.copy(xs = hole.xs, ys = hole.ys, zs = Range(hole.zs.max + 1, zs.max))
      ans.toSet
    }
  }

  case class Instruction(turnOn: Boolean, cuboid: Cuboid)

  def applyInstructions(instructions: List[Instruction]): Set[Cuboid] =
    instructions.foldLeft(Set.empty[Cuboid]) { case (cuboidsOn, Instruction(turnOn, cuboid)) =>
      cuboidsOn.foldLeft(if (turnOn) Set(cuboid) else Set.empty[Cuboid])((currOn, previous) => {
        previous.intersection(cuboid) match {
          case None       => currOn + previous
          case Some(hole) => currOn | previous.removeHole(hole)
        }
      })
    }

  def run(input: String): (Long, Long) = {
    val instructions = input
      .split("\n")
      .map({ case s"$status x=$x0..$x1,y=$y0..$y1,z=$z0..$z1" =>
        Instruction(
          status == "on",
          Cuboid(Range(x0.toLong, x1.toLong), Range(y0.toLong, y1.toLong), Range(z0.toLong, z1.toLong))
        )
      })
      .toList

    val part1 = applyInstructions(
      instructions.filter(instruction =>
        instruction.cuboid.xs.inside(Range(-50, 50)) && instruction.cuboid.ys
          .inside(Range(-50, 50)) && instruction.cuboid.zs.inside(Range(-50, 50))
      )
    ).toList.map(_.volume).sum

    val part2 = applyInstructions(instructions).toList.map(_.volume).sum

    (part1, part2)
  }
}
