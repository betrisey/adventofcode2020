object Day3 {
  def solve(inputs: List[String]): String = {
    inputs.zipWithIndex.count {
      case (s, i) => s.charAt((3*i) % s.length) == '#'
    }.toString
  }

  def solve2(inputs: List[String]): String = {
    val slopes = List((1,1), (3,1), (5,1), (7,1), (1, 2))
    (for (r, d) <- slopes yield
      inputs.zipWithIndex.count {
        case (s, i) if i % d == 0 => s.charAt((r*(i/d)) % s.length) == '#'
        case _ => false
      }).map(_.toLong).product.toString
  }
}