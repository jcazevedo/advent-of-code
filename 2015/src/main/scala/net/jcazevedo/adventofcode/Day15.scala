package net.jcazevedo.adventofcode

class Day15 extends DailyChallenge[Int, Int] {
  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  def run(filename: String): (Int, Int) = {
    val ingredients = io.Source.fromFile(filename).getLines.toList.map { line =>
      val Array(_, _, capacityS, _, durabilityS, _, flavorS, _, textureS, _, caloriesS) = line.split(" ")
      Ingredient(
        capacityS.substring(0, capacityS.size - 1).toInt,
        durabilityS.substring(0, durabilityS.size - 1).toInt,
        flavorS.substring(0, flavorS.size - 1).toInt,
        textureS.substring(0, textureS.size - 1).toInt,
        caloriesS.toInt)
    }

    def amounts(ingredients: List[Ingredient], remaining: Int): List[List[(Ingredient, Int)]] = {
      if (remaining == 0)
        List(ingredients.map(_ -> 0))
      else if (ingredients.size == 1)
        List(List((ingredients.head -> remaining)))
      else {
        (0 to remaining).flatMap { i =>
          val rem = amounts(ingredients.tail, remaining - i)
          rem.map { r =>
            (ingredients.head -> i) :: r
          }
        }.toList
      }
    }

    val seqs = amounts(ingredients, 100)

    def bestScore(seqs: List[List[(Ingredient, Int)]]) =
      seqs.map { seq =>
        val totCapacity = math.max(seq.map(v => v._1.capacity * v._2).sum, 0)
        val totDurability = math.max(seq.map(v => v._1.durability * v._2).sum, 0)
        val totFlavor = math.max(seq.map(v => v._1.flavor * v._2).sum, 0)
        val totTexture = math.max(seq.map(v => v._1.texture * v._2).sum, 0)

        totCapacity * totDurability * totFlavor * totTexture
      }.max

    val res1 = bestScore(seqs)
    val res2 = bestScore(seqs.filter { seq =>
      math.max(seq.map(v => v._1.calories * v._2).sum, 0) == 500
    })

    (res1, res2)
  }
}
