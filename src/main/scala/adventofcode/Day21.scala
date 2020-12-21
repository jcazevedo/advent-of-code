package adventofcode

class Day21 extends DailyChallenge[Int, String] {
  case class Food(ingredients: List[String], allergens: List[String])

  def getPossibleAllergents(foods: List[Food]): Map[String, Set[String]] =
    foods.foldLeft(Map.empty[String, Set[String]]) { case (acc, food) =>
      val ingredientSet = food.ingredients.toSet
      food.allergens.foldLeft(acc) { case (acc, allergen) =>
        acc.get(allergen) match {
          case Some(value) => acc.updated(allergen, value.intersect(ingredientSet))
          case None        => acc.updated(allergen, ingredientSet)
        }
      }
    }

  def part1(foods: List[Food]): Int = {
    val possibleAllergens = getPossibleAllergents(foods)
    val allIngredients = foods.flatMap(_.ingredients).toSet
    val safeIngredients = allIngredients.filter(food => !possibleAllergens.values.exists(_(food)))
    safeIngredients.toList.map(ingredient => foods.count(_.ingredients.contains(ingredient))).sum
  }

  def part2(foods: List[Food]): String = {
    val allergens = getPossibleAllergents(foods)

    def go(
        toAssign: Set[String],
        curr: Map[String, String] = Map.empty,
        usedI: Set[String] = Set.empty
    ): Option[Map[String, String]] =
      if (toAssign.isEmpty) Some(curr)
      else {
        val allergen = toAssign.head
        allergens(allergen).iterator
          .filter(!usedI(_))
          .map(assignment => go(toAssign - allergen, curr.updated(allergen, assignment), usedI + assignment))
          .find(_.nonEmpty)
          .flatten
      }

    val assignmentsOpt = go(allergens.keySet)
    assert(assignmentsOpt.nonEmpty)
    val assignments = assignmentsOpt.get
    assignments.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  def run(input: String): (Int, String) = {
    val foodRegex = raw"""([^\(]+?) \(contains ([^\)]+?)\)""".r
    val foods = input
      .split("\n")
      .map { case foodRegex(i, a) => Food(i.split(" ").toList, a.split(", ").toList) }
      .toList

    (part1(foods), part2(foods))
  }
}
