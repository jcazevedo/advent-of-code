object Day20 extends DailyChallenge[Int, Int] {
  case class Image(grid: Vector[String], background: Char)

  def lit(image: Image): Int =
    image.grid.map(_.count(_ == '#')).sum

  def toBinary(str: String): Int =
    str.map(ch => if (ch == '.') 0 else 1).foldLeft(0)(_ * 2 + _)

  def enhance(image: Image, enhancementAlgorithm: String): Image = {
    val height = image.grid.length
    val width = image.grid(0).length

    val targetHeight = height + 2
    val targetWidth = width + 2

    def get(i: Int, j: Int): Char =
      if (i >= 0 && i < height && j >= 0 && j < height) image.grid(i)(j) else image.background

    def getSquare(i: Int, j: Int): String =
      (-1 to 1).map(di => (-1 to 1).map(dj => get(i + di, j + dj)).mkString).mkString

    Image(
      (0 until targetHeight)
        .map(i => (0 until targetWidth).map(j => enhancementAlgorithm(toBinary(getSquare(i - 1, j - 1)))).mkString)
        .toVector,
      if (image.background == '.') enhancementAlgorithm.head else enhancementAlgorithm.last
    )
  }

  def run(input: String): (Int, Int) = {
    val lines = input.split("\n").map(_.trim).toList
    val imageEnhancementAlgorithm = lines.head
    val inputImage = Image(lines.drop(2).toVector, '.')

    val part1 = lit((0 until 2).foldLeft(inputImage)((img, _) => enhance(img, imageEnhancementAlgorithm)))
    val part2 = lit((0 until 50).foldLeft(inputImage)((img, _) => enhance(img, imageEnhancementAlgorithm)))

    (part1, part2)
  }
}
