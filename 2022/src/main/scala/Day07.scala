object Day07 extends DailyChallenge[Long, Long] {
  sealed trait Node
  case class Directory(contained: Map[String, Node] = Map.empty) extends Node {
    def add(node: Node, path: List[String]): Directory =
      add(node, path.head, path.tail)

    def add(node: Node, name: String, rest: List[String] = List.empty): Directory =
      if (rest.isEmpty) copy(contained = contained + (name -> node))
      else
        copy(contained = contained + (name -> (contained.get(name) match {
          case Some(d: Directory) => d.add(node, rest.head, rest.tail)
          case _                  => Directory().add(node, rest.head, rest.tail)
        })))
  }
  case class File(size: Long) extends Node

  def buildTree(input: List[String]): Directory = {
    var ans = Directory()

    def aux(commands: List[String], directories: List[String]): Unit = {
      if (commands.nonEmpty) {
        val currCommand = commands.head.substring(2, 4)
        if (currCommand == "cd") {
          val target = commands.head.drop(5)
          if (target == "/") aux(commands.tail, directories = List.empty)
          else if (target == "..") aux(commands.tail, directories.tail)
          else aux(commands.tail, target :: directories)
        } else if (currCommand == "ls") {
          val output = commands.tail.takeWhile(!_.startsWith("$"))
          output.foreach { line =>
            if (line.startsWith("dir")) {
              val name = line.stripPrefix("dir ")
              ans = ans.add(Directory(), (name :: directories).reverse)
            } else {
              val Array(size, name) = line.split("\\s+")
              ans = ans.add(File(size.toLong), (name :: directories).reverse)
            }
          }
          aux(commands.tail.dropWhile(!_.startsWith("$")), directories)
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
      case _ => Map.empty
    }

  def run(input: String): (Long, Long) = {
    val lines = input.split("\n").map(_.trim).toList

    val baseDir = buildTree(lines)
    val totalSpace = 70000000L
    val unusedNeeded = 30000000L
    val usedSpace = totalSize(baseDir)
    val target = totalSpace - unusedNeeded

    val part1 = getDirSizes(baseDir).filter(_._2 < 100000).values.sum
    val part2 = getDirSizes(baseDir).filter(usedSpace - _._2 < target).values.toList.sorted.head

    (part1, part2)
  }
}
