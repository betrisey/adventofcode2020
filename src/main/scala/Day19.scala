object Day19 {
  def solve(inputs: List[String]): String = {
    val inputRules = inputs.takeWhile(_ != "")
    val messages = inputs.dropWhile(_ != "").tail

    val rules = inputRules.map { input =>
      val Array(id, rule) = input.split(": ")
      (id.toInt, rule)
    }.sortBy(_._1).map(_._2).toArray

    val cache = scala.collection.mutable.Map.empty[Int, String]

    def buildRegex(rule: String): String = {
      if (rule.contains(" | ")) {
        rule.split(" \\| ")
          .map(buildRegex)
          .reduce((a, b) => s"($a)|($b)")
      } else if (rule == "\"a\"") {
        "a"
      } else if (rule == "\"b\"") {
        "b"
      } else if (rule.contains(" ")) {
        rule.split(" ")
          .map(part => buildRegex(part))
          .reduce((a, b) => s"($a)($b)")
      } else {
        val id = rule.toInt
        cache.getOrElseUpdate(id, buildRegex(rules(id)))
      }
    }

    val regex = ("^"+buildRegex(rules.head)+"$").r
    //println(regex)
    messages.count(regex.matches).toString
  }

  def solve2(inputs: List[String]): String = {
    val inputRules = inputs.takeWhile(_ != "")
    val messages = inputs.dropWhile(_ != "").tail

    val rules = inputRules.map { input =>
      val Array(id, rule) = input.split(": ")
      (id.toInt, rule)
    }.sortBy(_._1).map(_._2).toArray

    val cache = scala.collection.mutable.Map.empty[Int, String]

    def buildRegex(rule: String): String = {
      if (rule.contains(" | ")) {
        rule.split(" \\| ")
          .map(buildRegex)
          .reduce((a, b) => s"($a)|($b)")
      } else if (rule == "\"a\"") {
        "a"
      } else if (rule == "\"b\"") {
        "b"
      } else if (rule.contains(" ")) {
        rule.split(" ")
          .map(part => buildRegex(part))
          .reduce((a, b) => s"($a)($b)")
      } else if (rule == "8") {
        // 8: 42 | 42 8
        s"(${buildRegex("42")})+"
      } else if (rule == "11") {
        // Cannot use regex for this one
        // 11: 42 31 | 42 11 31
        // approximation using up to 100 repetitions
        val r42 = buildRegex("42")
        val r31 = buildRegex("31")
        (1 to 100).map(i => s"($r42){$i}($r31){$i}").mkString("|")
      } else {
        val id = rule.toInt
        cache.getOrElseUpdate(id, buildRegex(rules(id)))
      }
    }

    val regex = ("^"+buildRegex(rules.head)+"$").r
    //println(regex)
    messages.count(regex.matches).toString
  }
}
