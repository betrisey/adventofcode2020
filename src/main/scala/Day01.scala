object Day01 {
  def solve(input: List[String]): String = {
    val ints = input.map(_.toInt).zipWithIndex
    (for {
      (x, i) <- ints
      (y, j) <- ints
      if i != j
      if x + y == 2020
    } yield x*y).head.toString
  }

  def solve2(input: List[String]): String = {
    val ints = input.map(_.toInt).zipWithIndex
    (for {
      (x, i) <- ints
      (y, j) <- ints
      (z, k) <- ints
      if i != j && j != k && k != i
      if x + y + z == 2020
    } yield x*y*z).head.toString
  }
}