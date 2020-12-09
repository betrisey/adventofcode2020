object Day9 {
  def solve(inputs: List[String]): String = {
    val numbers = inputs.map(_.toLong)
    
    def isWrong(list: List[Long], sum: Long): Boolean = {
      (for {
        i <- list
        j <- list
        if i != j && i + j == sum
      } yield sum).isEmpty
    }

    numbers.sliding(26).collectFirst {
      case list: List[Long] if isWrong(list.init, list.last) => list.last
    }.get.toString
  }

  def solve2(inputs: List[String]): String = {
    val numbers = inputs.map(_.toLong)
    val sum = solve(inputs).toLong

    /* Shorter less but O(N^3)
    (for {
      size <- 2 to numbers.size
      slide <- numbers.sliding(size)
      if slide.sum == sum
    } yield slide.min + slide.max).head.toString
    */

    // O(N^2) but longer
    val list = (for {
      start <- 0 until numbers.size - 1
      scan <- numbers.drop(start).scanLeft((0L, 0))((acc, x) => (acc._1 + x, acc._2 + 1))
      if scan._1 == sum
    } yield numbers.drop(start).take(scan._2)).head
    (list.min + list.max).toString
  }
}