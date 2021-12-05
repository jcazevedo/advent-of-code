package adventofcode

class Day23 extends DailyChallenge[String, Long] {
  def simulate(str: String, turns: Int, idx: Int = 0): String =
    if (turns == 0)
      str.substring(str.indexOf('1')) + str.substring(0, str.indexOf('1'))
    else {
      val curr = str(idx)
      val L = str.length
      val taken = Iterator.iterate((idx + 1) % L)(prev => (prev + 1) % L).take(3).map(str).mkString
      val remaining = str.filter(ch => !taken.contains(ch))
      lazy val minCh = remaining.min
      lazy val maxCh = remaining.max

      def getTarget(v: Char): Int =
        if (remaining.contains(v)) remaining.indexOf(v.toInt)
        else {
          val next = (v - 1).toChar
          getTarget(if (next < minCh) maxCh else next)
        }

      val target = getTarget((curr - 1).toChar)
      val next =
        remaining.substring(0, target + 1) + taken + (if (target + 1 < remaining.length) remaining.substring(target + 1)
                                                      else "")

      simulate(next, turns - 1, (next.indexOf(curr.toInt) + 1) % L)
    }

  def part1(cups: String): String = {
    simulate(cups, 100).tail
  }

  case class Node(value: Int) {
    var next: Node = _
  }

  def part2(cups: String): Long = {
    val maxValue = 1000000
    val cupIndices = cups.map(_ - '0')
    val nodes = (cupIndices.map(Node) ++ (cupIndices.max + 1 to maxValue).map(Node)).toVector
    nodes.sliding(2).foreach { case Vector(n1, n2) => n1.next = n2 }
    nodes.last.next = nodes.head
    val start = nodes.head
    val sortedNodes = nodes.sortBy(_.value)

    def simulate(curr: Node, turns: Int): Unit = {
      if (turns > 0) {
        val slice = Iterator.iterate(curr.next)(prev => prev.next).take(3).toList
        val sliceNumbers = slice.map(_.value)
        val nextCurr = slice.last.next
        curr.next = nextCurr

        def getTarget(v: Int): Int = {
          val target = if (v == 1) maxValue else v - 1
          if (sliceNumbers.contains(target))
            getTarget(target)
          else
            target
        }

        val targetIdx = getTarget(curr.value)
        val targetNode = sortedNodes(targetIdx - 1)
        val targetNodeNext = targetNode.next
        targetNode.next = slice.head
        slice.last.next = targetNodeNext

        simulate(curr.next, turns - 1)
      }
    }

    simulate(start, 10000000)

    val n1 = sortedNodes(0).next
    val n2 = n1.next

    n1.value.toLong * n2.value.toLong
  }

  def run(input: String): (String, Long) = {
    val cups = input.trim
    (part1(cups), part2(cups))
  }
}
