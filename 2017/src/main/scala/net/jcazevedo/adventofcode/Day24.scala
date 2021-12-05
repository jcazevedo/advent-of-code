package net.jcazevedo.adventofcode

class Day24 extends DailyChallenge[Int, Int] {
  case class Component(v1: Int, v2: Int) {
    lazy val strength = v1 + v2
    def swap = Component(v2, v1)
  }

  def run(filename: String): (Int, Int) = {
    val components: List[Component] = io.Source.fromFile(filename).getLines().map { l =>
      val ll = l.split("/")
      Component(ll(0).toInt, ll(1).toInt)
    }.toList

    def genBridges(components: List[Component]): List[List[Component]] = {
      def aux(v: Int, components: List[Component]): List[List[Component]] =
        if (components.isEmpty)
          Nil
        else {
          val goodHeads = components.filter(c => c.v1 == v || c.v2 == v)
          goodHeads.flatMap { h =>
            val rest = components diff List(h)
            val rem = if (h.v1 == v)
              aux(h.v2, rest).map(h :: _)
            else
              aux(h.v1, rest).map(h.swap :: _)

            if (rem.isEmpty)
              List(List(h), List(h.swap))
            else
              rem
          }
        }

      aux(0, components)
    }

    def bridgeStrength(b: List[Component]): Int = b.map(_.strength).sum

    val bridges = genBridges(components)
    val res1 = bridges.map(bridgeStrength).max
    val maxLength = bridges.map(_.length).max
    val res2 = bridges.filter(_.length == maxLength).map(bridgeStrength).max

    (res1, res2)
  }
}
