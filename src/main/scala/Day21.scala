object Day21 {
  def parseLine(line: String): (Set[String], List[String]) = {
    val pattern = raw"([a-z ]+) \(contains ([a-z, ]+)\)".r
    val pattern(ingredients, allergens) = line
    (ingredients.split(" ").toSet, allergens.split(", ").toList)
  }

  def solve(inputs: List[String]): String = {
    val list = inputs.map(parseLine)
    val allergenToIngredients = list.flatMap { case (ingredients, allergens) =>  allergens.map(allergen => (allergen, ingredients))}
    val allergenToPossibleIngredients = allergenToIngredients.groupBy(_._1).view.mapValues(_.map(_._2).reduce((a, b) => a intersect b)).toMap

    def findMatch(map: Map[String, Set[String]], matches: Map[String, String]): Map[String, String] = {
      if map.isEmpty then
        matches
      else
        val Some(allergen, ingredientSingleton) = map.find(_._2.size == 1)
        val ingredient = ingredientSingleton.head
        findMatch((map - allergen).view.mapValues(_ - ingredient).toMap, matches + (allergen -> ingredient))
    }

    val matches = findMatch(allergenToPossibleIngredients, Map.empty)
    
    val allergenIngredients = matches.values.toSet

    val allIngredients = list.flatMap(_._1.toList)
    val answer1 = allIngredients.count(!allergenIngredients.contains(_))
    
    val answer2 = matches.toList.sortBy(_._1).map(_._2).mkString(",")
    s"answer1: $answer1\nanswer2: $answer2"
  }

  def solve2(inputs: List[String]): String = solve(inputs)
}
