object Day5 {
  def parseID(id: String): Int = {
    Integer.parseInt(id.replace("F", "0")
                       .replace("B", "1")
                       .replace("L", "0")
                       .replace("R", "1"), 2)
  }

  def solve(inputs: List[String]): String = {
    inputs.map(parseID).max.toString
  }

  def solve2(inputs: List[String]): String = {
    val taken = inputs.map(parseID).toSet

    (for {
      i <- taken.min to taken.max
      if !taken.contains(i)
    } yield i).head.toString
  }
}