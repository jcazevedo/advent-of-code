package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day14 extends DailyChallenge[Int, Int] {
  def run(filename: String): (Int, Int) = {
    def hexToBin(input: String): String =
      input.flatMap { char =>
        var res = Integer.toBinaryString(Integer.parseInt("" + char, 16))
        while (res.length < 4)
          res = "0" + res
        res
      }

    def knotHash(input: String): String = {
      def reverse(ns: IndexedSeq[Int], i: Int, j: Int): IndexedSeq[Int] =
        if (i > j)
          ns
        else {
          val ni = i % ns.length
          val nj = j % ns.length
          reverse(ns.updated(nj, ns(ni)).updated(ni, ns(nj)), i + 1, j - 1)
        }

      def go(ns: IndexedSeq[Int], ls: Seq[Int], position: Int = 0, skipSize: Int = 0): (IndexedSeq[Int], Int, Int) =
        if (ls.isEmpty)
          (ns, position, skipSize)
        else
          go(reverse(ns, position, position + ls.head - 1), ls.tail, position + ls.head + skipSize, skipSize + 1)

      def xorAll(l: Seq[Int]): Int =
        if (l.size == 1) l.head
        else l.head ^ xorAll(l.tail)

      val numbers = (0 to 255)
      val lengths = input.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)

      val (sparseHash, _, _) =
        (0 until 64).foldLeft[(IndexedSeq[Int], Int, Int)]((numbers, 0, 0)) {
          case ((currSeq, currPos, currSkip), _) =>
            go(currSeq, lengths, currPos, currSkip)
        }

      sparseHash.sliding(16, 16).map(xorAll).map { v =>
        var res = v.toHexString
        while (res.length < 2)
          res = "0" + res
        res
      }.mkString
    }

    val input = io.Source.fromFile(filename).getLines().next()
    val square: Seq[String] = (0 to 127).map(v => s"$input-$v").map(knotHash).map(hexToBin)
    val res: Int = square.flatten.count(_ == '1')

    val h = square.length
    val w = square.head.length
    val visited = mutable.Set[(Int, Int)]()
    val diffs = Seq((-1, 0), (1, 0), (0, 1), (0, -1))

    def visit(i: Int, j: Int): Unit =
      if (i >= 0 && i < h && j >= 0 && j < w && square(i)(j) == '1' && !visited((i, j))) {
        visited.add((i, j))
        diffs.foreach { case (di, dj) => visit(i + di, j + dj) }
      }

    var nRegions = 0

    (0 until h).foreach { i =>
      (0 until w).foreach { j =>
        if (square(i)(j) == '1' && !visited((i, j))) {
          nRegions += 1
          visit(i, j)
        }
      }
    }

    (res, nRegions)
  }
}
