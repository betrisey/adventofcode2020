object Day7 {
  def parseRule(rule: String): (String, Set[(Int, String)]) = {
    var Array(h, t) = rule.split(" bags contain ")
    val format = raw"(\d+) ([a-z]+ [a-z]+) bags?".r
    val list = t.init.split(", ").collect {
      case format(count, color) => (count.toInt, color)
    }.toSet
    (h, list)
  }
  
  def solve(inputs: List[String]): String = {
    val rules = inputs.map(parseRule)
    
    def findContainers(content: Set[String]): Set[String] = {
      rules.filter(_._2.map(_._2).intersect(content).nonEmpty).map(_._1).toSet
    }

    val init = findContainers(Set("shiny gold"))

    def expand(set: Set[String]): Set[String] = {
      val newSet = findContainers(set) union set
      if (newSet == set)
        set
      else
        expand(newSet)
    }

    expand(init).size.toString
  }

  def solve2(inputs: List[String]): String = {
    val rules = inputs.map(parseRule).toMap
    
    def count(bag: String): Int = {
      rules(bag)
        .toList // Convert into list otherwise we wouldn't count two bags with the same number
        .map(x => x._1 * (1 + count(x._2))).sum
    }

    count("shiny gold").toString
  }
}