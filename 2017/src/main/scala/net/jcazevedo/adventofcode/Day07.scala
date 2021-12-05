package net.jcazevedo.adventofcode

import scala.collection.mutable

class Day07 extends DailyChallenge[String, Int] {
  def run(filename: String): (String, Int) = {
    val lines = io.Source.fromFile(filename).getLines.toList

    val weight = mutable.Map[String, Int]()
    val parent = mutable.Map[String, String]()
    val nodes = mutable.Set[String]()
    val children = mutable.Map[String, List[String]]()

    lines.foreach { line =>
      val ss = line.split("\\s+")
      nodes += ss(0)
      weight(ss(0)) = ss(1).drop(1).dropRight(1).toInt
      if (ss.length > 2) {
        (3 until ss.length).foreach { idx =>
          val n = if (ss(idx).endsWith(",")) ss(idx).dropRight(1) else ss(idx)
          nodes += n
          parent(n) = ss(0)
          children(ss(0)) = n :: children.getOrElse(ss(0), Nil)
        }
      }
    }

    var base = nodes.find(n => !parent.contains(n))
    val res1 = base.get

    val weightCache = mutable.Map[String, Int]()

    def programWeight(program: String): Int =
      if (weightCache.contains(program))
        weightCache(program)
      else {
        val w = weight(program) + children.getOrElse(program, Nil).map(programWeight).sum
        weightCache(program) = w
        w
      }

    def balanced(program: String): Boolean = {
      val weights = children.getOrElse(program, Nil).map(programWeight)
      weights.zip(weights.drop(1)).forall { case (v1, v2) => v1 == v2 }
    }

    def treeIterator(program: String): Iterator[String] = {
      Iterator(program) ++ children.getOrElse(program, Nil).flatMap(treeIterator)
    }

    val lastUnbalanced = treeIterator(base.get).filter(!balanced(_)).toList.last
    val weights = children(lastUnbalanced).map(ch => ch -> programWeight(ch))
      .toMap.groupBy(_._2).mapValues(_.map(_._1).toList).toList

    val balancedWeight = weights.filter(_._2.length != 1).head._1
    val (unbalancedProgram, unbalancedWeight) = {
      val l = weights.filter(_._2.length == 1).head
      (l._2.head, l._1)
    }

    val res2 = weight(unbalancedProgram) + (balancedWeight - unbalancedWeight)

    (res1, res2)
  }
}
