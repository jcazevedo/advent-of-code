package net.jcazevedo.adventofcode

class Day24 extends DailyChallenge[Long, Long] {
  def getGroupsOfWeightAndSize(p: Seq[Int], w: Int, s: Int): Iterator[Seq[Int]] = {
    p.combinations(s).filter(_.sum == w)
  }

  def getValidCombinations(p: Seq[Int], w: Int): Iterator[(Seq[Int], Seq[Int], Seq[Int])] = {
    for {
      s1 <- (1 to p.size).iterator
      p1 <- getGroupsOfWeightAndSize(p, w, s1)
      r1 = p diff p1
      s2 <- (1 to (p.size - s1)).iterator
      p2 <- getGroupsOfWeightAndSize(r1, w, s2)
      r2 = r1 diff p2
      s3 = (p.size - s1 - s2)
      p3 <- getGroupsOfWeightAndSize(r2, w, s3)
    } yield (p1, p2, p3)
  }

  def getValidCombinationsP2(p: Seq[Int], w: Int): Iterator[(Seq[Int], Seq[Int], Seq[Int], Seq[Int])] = {
    for {
      s1 <- (1 to p.size).iterator
      p1 <- getGroupsOfWeightAndSize(p, w, s1)
      r1 = p diff p1
      s2 <- (1 to (p.size - s1)).iterator
      p2 <- getGroupsOfWeightAndSize(r1, w, s2)
      r2 = r1 diff p2
      s3 <- (1 to (p.size - s1 - s2)).iterator
      p3 <- getGroupsOfWeightAndSize(r2, w, s3)
      r3 = r2 diff p3
      s4 = (p.size - s1 - s2 - s3)
      p4 <- getGroupsOfWeightAndSize(r3, w, s4)
    } yield (p1, p2, p3, p4)
  }

  def run(filename: String): (Long, Long) = {
    val presents = io.Source.fromFile(filename).getLines.toList.map(_.toInt).sorted
    val totWeight = presents.sum

    (getValidCombinations(presents, totWeight / 3).next._1.map(_.toLong).reduceLeft(_ * _),
      getValidCombinationsP2(presents, totWeight / 4).next._1.map(_.toLong).reduceLeft(_ * _))
  }
}
