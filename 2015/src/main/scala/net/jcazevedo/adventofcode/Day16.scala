package net.jcazevedo.adventofcode

class Day16 extends DailyChallenge[Int, Int] {
  case class Sue(id: Int,
                 children: Option[Int],
                 cats: Option[Int],
                 samoyeds: Option[Int],
                 pomeranians: Option[Int],
                 akitas: Option[Int],
                 vizslas: Option[Int],
                 goldfish: Option[Int],
                 trees: Option[Int],
                 cars: Option[Int],
                 perfumes: Option[Int])

  def run(filename: String): (Int, Int) = {
    val sues = io.Source.fromFile(filename).getLines.toList.map { line =>
      val splits = line.split(" ")
      val id = splits(1).dropRight(1).toInt
      val evidences = splits.drop(2).sliding(2).zipWithIndex.filter(_._2 % 2 == 0).map(_._1).map {
        case Array(k, v) =>
          k.dropRight(1) -> (if (v.last == ',') v.dropRight(1).toInt else v.toInt)
      }.toMap

      Sue(
        id,
        evidences.get("children"),
        evidences.get("cats"),
        evidences.get("samoyeds"),
        evidences.get("pomeranians"),
        evidences.get("akitas"),
        evidences.get("vizslas"),
        evidences.get("goldfish"),
        evidences.get("trees"),
        evidences.get("cars"),
        evidences.get("perfumes"))
    }

    val knownSue = Sue(
      -1, Some(3), Some(7), Some(2), Some(3), Some(0), Some(0), Some(5),
      Some(3), Some(2), Some(1))

    val valid1 = sues.filter { sue =>
      (sue.children.isEmpty || sue.children.get == knownSue.children.get) &&
        (sue.cats.isEmpty || sue.cats.get == knownSue.cats.get) &&
        (sue.samoyeds.isEmpty || sue.samoyeds.get == knownSue.samoyeds.get) &&
        (sue.pomeranians.isEmpty || sue.pomeranians.get == knownSue.pomeranians.get) &&
        (sue.akitas.isEmpty || sue.akitas.get == knownSue.akitas.get) &&
        (sue.vizslas.isEmpty || sue.vizslas.get == knownSue.vizslas.get) &&
        (sue.goldfish.isEmpty || sue.goldfish.get == knownSue.goldfish.get) &&
        (sue.trees.isEmpty || sue.trees.get == knownSue.trees.get) &&
        (sue.cars.isEmpty || sue.cars.get == knownSue.cars.get) &&
        (sue.perfumes.isEmpty || sue.perfumes.get == knownSue.perfumes.get)
    }

    val valid2 = sues.filter { sue =>
      (sue.children.isEmpty || sue.children.get == knownSue.children.get) &&
        (sue.cats.isEmpty || sue.cats.get > knownSue.cats.get) &&
        (sue.samoyeds.isEmpty || sue.samoyeds.get == knownSue.samoyeds.get) &&
        (sue.pomeranians.isEmpty || sue.pomeranians.get < knownSue.pomeranians.get) &&
        (sue.akitas.isEmpty || sue.akitas.get == knownSue.akitas.get) &&
        (sue.vizslas.isEmpty || sue.vizslas.get == knownSue.vizslas.get) &&
        (sue.goldfish.isEmpty || sue.goldfish.get < knownSue.goldfish.get) &&
        (sue.trees.isEmpty || sue.trees.get > knownSue.trees.get) &&
        (sue.cars.isEmpty || sue.cars.get == knownSue.cars.get) &&
        (sue.perfumes.isEmpty || sue.perfumes.get == knownSue.perfumes.get)
    }

    (valid1.head.id, valid2.head.id)
  }
}
