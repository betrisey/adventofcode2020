object Day4 {
  def parsePassports(inputs: List[String]): List[Map[String, String]] = {
    def group(inputs: List[Array[String]]): List[List[String]] = {
      inputs.span(_.head != "") match
        case (head, _ :: tail) => head.flatten :: group(tail)
        case (head, Nil) => head.flatten :: Nil
        case (Nil, Nil) => Nil
    }
    group(inputs.map(_.split(" ")))
        .map(p => p.map {
          x => val Array(k,v) = x.split(":"); k -> v
        }.toMap)
  }

  def solve(inputs: List[String]): String = {
    val passports = parsePassports(inputs)
    val fields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    passports.count(p => fields.forall(p.contains(_))).toString
  }

  def solve2(inputs: List[String]): String = {
    val passports = parsePassports(inputs)
    
    def inRange(min: Int, max: Int)(s: String): Boolean = {
      s.toIntOption.map(i => i >= min && i <= max).getOrElse(false)
    }

    import scala.util.matching.Regex
    def regex(reg: Regex)(s: String): Boolean = {
      reg.pattern.matcher(s).matches
    }

    val rules = List(
      "byr" -> inRange(1920, 2002),
      "iyr" -> inRange(2010, 2020),
      "eyr" -> inRange(2020, 2030),
      "hgt" -> ((value: String) =>
          value.endsWith("cm") && inRange(150, 193)(value.substring(0, value.length - 2))||
          value.endsWith("in") && inRange(59, 76)(value.substring(0, value.length - 2))
        ),
      "hcl" -> regex("#[0-9a-f]{6}".r),
      "ecl" -> regex("amb|blu|brn|gry|grn|hzl|oth".r),
      "pid" -> regex("[0-9]{9}".r)
    )
    
    passports.count(p => rules.forall(r => p.contains(r._1) && r._2(p(r._1)))).toString
  }
}