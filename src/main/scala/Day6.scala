object Day6 {
  def parseGroups(inputs: List[String]): List[List[Set[Char]]] = {
    def group(inputs: List[Set[Char]]): List[List[Set[Char]]] = {
      inputs.span(_.nonEmpty) match
        case (head, _ :: tail) => head :: group(tail)
        case (head, Nil) => head :: Nil
    }
    group(inputs.map(_.toSet))
  }

  def solve(inputs: List[String]): String = {
    /*inputs.map(x => if x == "" then "|" else x)
      .mkString
      .split("|")
      .map(_.toSet.size)
      .sum
      .toString*/
    parseGroups(inputs).map(_.reduce((x,y) => x union y).size).sum.toString
  }

  def solve2(inputs: List[String]): String = {
    parseGroups(inputs).map(_.reduce((x,y) => x intersect y).size).sum.toString
  }
}