object Day07 extends DailyChallenge[Long, Long] {
  sealed trait Node

  case class File(size: Long) extends Node

  case class Directory(contained: Map[String, Node] = Map.empty) extends Node {
    def add(node: Node, path: List[String]): Directory =
      path match {
        case Nil =>
          this

        case h :: Nil =>
          copy(contained = contained + (h -> node))

        case h :: t =>
          copy(contained = contained + (h -> (contained.get(h) match {
            case Some(d: Directory) => d.add(node, t)
            case _                  => Directory().add(node, t)
          })))
      }
  }

  def buildTree(input: List[String]): Directory = {
    var ans = Directory()

    def aux(commands: List[String], directories: List[String]): Unit = {
      if (commands.nonEmpty) {
        val currCommand = commands.head.substring(2, 4)
        currCommand match {
          case "cd" =>
            val target = commands.head.drop(5)
            if (target == "/") aux(commands.tail, directories = List.empty)
            else if (target == "..") aux(commands.tail, directories.tail)
            else aux(commands.tail, target :: directories)

          case "ls" =>
            val output = commands.tail.takeWhile(!_.startsWith("$"))
            output.foreach(line =>
              if (line.startsWith("dir")) {
                val name = line.stripPrefix("dir ")
                ans = ans.add(Directory(), (name :: directories).reverse)
              } else {
                val Array(size, name) = line.split("\\s+")
                ans = ans.add(File(size.toLong), (name :: directories).reverse)
              }
            )
            aux(commands.tail.drop(output.length), directories)
        }
      }
    }

    aux(input, List.empty)

    ans
  }

  def totalSize(n: Node): Long =
    n match {
      case Directory(contained) => contained.values.map(totalSize).sum
      case File(size)           => size
    }

  def getDirSizes(n: Node, curr: Vector[String] = Vector.empty): Map[List[String], Long] =
    n match {
      case d @ Directory(contained) =>
        Map(curr.toList -> totalSize(d)) ++ contained.flatMap({ case (k, v) => getDirSizes(v, curr :+ k) })

      case _ =>
        Map.empty
    }

  def run(input: String): (Long, Long) = {
    val lines = input.split("\n").map(_.trim).toList

    val baseDir = buildTree(lines)
    val usedSpace = totalSize(baseDir)

    val part1 = getDirSizes(baseDir).filter(_._2 < 100000L).values.sum
    val part2 = getDirSizes(baseDir).filter(usedSpace - _._2 < 40000000L).values.toList.min

    (part1, part2)
  }
}
