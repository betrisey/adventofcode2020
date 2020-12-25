object Day25 {
  def solve(inputs: List[String]): String = {
    val pkA = inputs(0).toInt
    val pkB = inputs(1).toInt
    
    val base = 7
    val mod = 20201227

    val skB = LazyList.from(1)
      .scanLeft((1, 0))((x, i) => ((x._1*base)%mod, i))
      .find(_._1 == pkB)
      .get._2
    
    BigInt(pkA).modPow(skB, mod).toString
  }

  def solve2(inputs: List[String]): String = {
    ""
  }
}
