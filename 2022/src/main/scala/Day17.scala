import scala.collection.mutable

object Day17 extends DailyChallenge[Long, Long] {
  case class Point(i: Int, j: Int) {
    def +(other: Point): Point = Point(i + other.i, j + other.j)
  }

  final def Rocks: LazyList[Vector[String]] =
    List(
      Vector("####"),
      Vector(".#.", "###", ".#."),
      Vector("..#", "..#", "###"),
      Vector("#", "#", "#", "#"),
      Vector("##", "##")
    ).to(LazyList) #::: Rocks

  final val Down: Point = Point(-1, 0)
  final val Left: Point = Point(0, -1)
  final val Right: Point = Point(0, +1)

  def tallAfter(rocks: LazyList[Vector[String]], jetPattern: LazyList[(Char, Int)], nRocks: Long, width: Int): Long = {
    val p = jetPattern.iterator.buffered
    val r = rocks.map(_.reverse).iterator

    def collides(rock: Vector[String], bottomLeft: Point, state: Vector[String]): Boolean =
      bottomLeft.j < 0 || (bottomLeft.j + rock.head.length - 1) >= width || rock.zipWithIndex.exists { case (line, i) =>
        val ni = bottomLeft.i + i
        ni < 0 || ni < state.length && line.zipWithIndex.exists { case (ch, j) =>
          val nj = bottomLeft.j + j
          ch == '#' && state(ni)(nj) == '#'
        }
      }

    def move(rock: Vector[String], bottomLeft: Point, diff: Point, state: Vector[String]): Point = {
      val next = bottomLeft + diff
      if (collides(rock, next, state)) bottomLeft else next
    }

    val cache = mutable.Map.empty[(Vector[String], Vector[String], Int), (Long, Long)]

    def aux(length: Long, state: Vector[String], rock: Vector[String], rocksLeft: Long): Long = {
      if (rocksLeft == 0) length
      else {
        val nextMove = p.head

        var current = Point(state.length + 4, 2)
        var next = move(rock, current, Down, state)

        while (current != next) {
          current = next
          current = move(rock, current, if (p.next._1 == '<') Left else Right, state)
          next = move(rock, current, Down, state)
        }

        val diff = math.max((current.i + rock.length) - state.length, 0)
        val withMoreSpace = state ++ Vector.fill(diff)("." * width)
        val nextState = rock.zipWithIndex.foldLeft(withMoreSpace) { case (state, (line, i)) =>
          line.zipWithIndex.foldLeft(state) { case (state, (ch, j)) =>
            val ni = i + current.i
            val nj = j + current.j
            if (ch == '#') state.updated(ni, state(ni).updated(nj, ch)) else state
          }
        }

        val stateDiff = nextState.drop(current.i)
        val lengthDiff = nextState.length - state.length
        val nextRocksLeft = rocksLeft - 1
        val rocksDropped = nRocks - nextRocksLeft

        if (!cache.contains((rock, stateDiff, nextMove._2))) {
          cache((rock, stateDiff, nextMove._2)) = (length + lengthDiff, rocksDropped)
          aux(length + lengthDiff, nextState, r.next, nextRocksLeft)
        } else {
          val (prevLength, prevRocksDropped) = cache((rock, stateDiff, nextMove._2))
          val rocksAtLoop = rocksDropped - prevRocksDropped
          val nLoops = (nRocks - prevRocksDropped) / rocksAtLoop
          val loopLength = (length + lengthDiff - prevLength)
          val additionalLength = nLoops * loopLength

          aux(
            prevLength + additionalLength,
            nextState,
            r.next,
            nextRocksLeft % rocksAtLoop
          )
        }
      }
    }

    aux(0L, Vector.empty[String], r.next, nRocks)
  }

  def run(input: String): (Long, Long) = {
    val pattern = input.trim.to(LazyList).zipWithIndex
    def jetPattern: LazyList[(Char, Int)] = pattern #::: jetPattern

    val part1 = tallAfter(Rocks, jetPattern, nRocks = 2022L, width = 7)
    val part2 = tallAfter(Rocks, jetPattern, nRocks = 1000000000000L, width = 7)

    (part1, part2)
  }
}
