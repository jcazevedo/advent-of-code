import scala.collection.mutable

object Day19 extends DailyChallenge[Int, Int] {
  case class Beacon(x: Int, y: Int, z: Int) {
    lazy val permutations: List[Beacon] = {
      List(
        Beacon(x, y, z),
        Beacon(y, z, x),
        Beacon(z, x, y),
        Beacon(-x, z, y),
        Beacon(z, y, -x),
        Beacon(y, -x, z),
        Beacon(x, z, -y),
        Beacon(z, -y, x),
        Beacon(-y, x, z),
        Beacon(x, -z, y),
        Beacon(-z, y, x),
        Beacon(y, x, -z),
        Beacon(-x, -y, z),
        Beacon(-y, z, -x),
        Beacon(z, -x, -y),
        Beacon(-x, y, -z),
        Beacon(y, -z, -x),
        Beacon(-z, -x, y),
        Beacon(x, -y, -z),
        Beacon(-y, -z, x),
        Beacon(-z, x, -y),
        Beacon(-x, -z, -y),
        Beacon(-z, -y, -x),
        Beacon(-y, -x, -z)
      )
    }

    def +(other: Beacon): Beacon = Beacon(x + other.x, y + other.y, z + other.z)
    def -(other: Beacon): Beacon = Beacon(x - other.x, y - other.y, z - other.z)
    def manhattanDistance(other: Beacon): Int = math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)
  }

  case class Scanner(id: Int, beacons: List[Beacon]) {
    lazy val permutations: List[Scanner] = {
      val nPermutations = beacons.head.permutations.length
      beacons
        .map(_.permutations)
        .foldRight(List.fill(nPermutations)(List.empty[Beacon]))((curr, next) =>
          curr.zip(next).map({ case (p, l) => p :: l })
        )
        .map(permutations => Scanner(id, permutations))
    }

    def overlaps(other: Scanner): Option[Beacon] = {
      (for {
        thisBeacon <- beacons.iterator
        thisDeltas = beacons.map(_ - thisBeacon).toSet
        thatBeacon <- other.beacons.iterator
        thatDeltas = other.beacons.map(_ - thatBeacon).toSet
        if thisDeltas.intersect(thatDeltas).size >= 12
      } yield (thisBeacon - thatBeacon)).nextOption
    }
  }

  def getBeaconsAndScanners(scanners: List[Scanner]): (Set[Beacon], Map[Int, Beacon]) = {
    val beacons = mutable.Set.empty[Beacon]
    val includedScanners = mutable.Map.empty[Int, Scanner]
    val diffToStart = mutable.Map.empty[Int, Beacon]
    includedScanners(scanners.head.id) = scanners.head
    scanners.head.beacons.foreach(beacons += _)
    diffToStart(scanners.head.id) = Beacon(0, 0, 0)

    val q = mutable.Queue.empty[Scanner]
    q.enqueue(scanners.head)

    while (q.nonEmpty) {
      val current = q.dequeue()

      val candidates = (for {
        other <- scanners.filter(scanner => !includedScanners.contains(scanner.id)).flatMap(_.permutations)
        diff <- current.overlaps(other)
      } yield (other, diff))

      candidates.foreach({ case (nextToInclude, diffToIncluded) =>
        includedScanners(nextToInclude.id) = nextToInclude
        diffToStart(nextToInclude.id) = diffToStart(current.id) + diffToIncluded
        nextToInclude.beacons.foreach(nextBeacon => beacons += (nextBeacon + diffToStart(nextToInclude.id)))
        q.enqueue(nextToInclude)
      })
    }

    (beacons.toSet, diffToStart.toMap)
  }

  def run(input: String): (Int, Int) = {
    val scanners = input
      .split("\n")
      .map(_.trim)
      .foldLeft(Vector.empty[Scanner])((scanners, line) =>
        if (line.startsWith("---"))
          scanners :+ Scanner(line.stripPrefix("--- scanner ").stripSuffix(" ---").toInt, List.empty)
        else if (line.nonEmpty) {
          val Array(x, y, z) = line.split(",")
          val point = Beacon(x.toInt, y.toInt, z.toInt)
          val lastScanner = scanners.last
          scanners.init :+ lastScanner.copy(beacons = lastScanner.beacons :+ point)
        } else scanners
      )
      .toList

    val (beaconPos, scannerPos) = getBeaconsAndScanners(scanners)

    val part1 = beaconPos.size
    val part2 = (for {
      s1 <- scannerPos.values
      s2 <- scannerPos.values
    } yield s1.manhattanDistance(s2)).max

    (part1, part2)
  }
}
