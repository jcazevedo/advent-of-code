import scala.collection.mutable

object Day05 extends DailyChallenge[Int, Int] {
  case class Rule(page1: Int, page2: Int)
  case class Update(pages: Vector[Int])
  case class Input(rules: Vector[Rule], updates: Vector[Update])

  def readInput(input: String): Input = {
    val rules = Vector.newBuilder[Rule]
    val updates = Vector.newBuilder[Update]

    val lines = input.split("\n")

    var inRules = true
    lines.foreach(line =>
      if (line.isEmpty)
        inRules = false
      else if (inRules)
        rules.addOne({
          val Array(p1, p2) = line.split("\\|")
          Rule(p1.toInt, p2.toInt)
        })
      else
        updates.addOne(Update(line.split(",").map(_.toInt).toVector))
    )

    Input(rules.result(), updates.result())
  }

  def run(input: String): (Int, Int) = {
    val Input(rules, updates) = readInput(input)

    def rightOrder(update: Update): Boolean =
      update.pages.tails.forall(pages =>
        pages.size <= 1 || {
          val p = pages.head
          pages.tail.forall(nextPage => !rules.exists(_ == Rule(nextPage, p)))
        }
      )

    def middlePageNumber(update: Update): Int =
      update.pages(update.pages.length / 2)

    def reOrder(update: Update): Update = {
      val graph = {
        val builder = Map.newBuilder[Int, Vector[Int]]
        update.pages.foreach(p => {
          val edges = rules.filter(rule => rule.page1 == p && update.pages.contains(rule.page2))
          builder.addOne((p, edges.map(_.page2).toVector))
        })
        builder.result()
      }

      val inDegree = mutable.Map.empty[Int, Int]
      graph.foreach((_, adj) =>
        adj.foreach(node =>
          inDegree.updateWith(node)({
            case None      => Some(1)
            case Some(deg) => Some(deg + 1)
          })
        )
      )

      val q = mutable.Queue.empty[Int]
      update.pages.foreach(page => if (inDegree.getOrElse(page, 0) == 0) q.enqueue(page))

      val newOrder = Vector.newBuilder[Int]
      while (!q.isEmpty) {
        val page = q.dequeue()
        newOrder.addOne(page)

        graph
          .getOrElse(page, Vector.empty[Int])
          .foreach(v => {
            inDegree.updateWith(v)({
              case Some(1) =>
                q.enqueue(v)
                None
              case Some(deg) =>
                Some(deg - 1)
              case None =>
                None
            })
          })
      }

      Update(newOrder.result())
    }

    val part1 = updates.filter(rightOrder).map(middlePageNumber).sum
    val part2 = updates.filter(!rightOrder(_)).map(reOrder).map(middlePageNumber).sum

    (part1, part2)
  }
}
