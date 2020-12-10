object Day10 {
  def solve(inputs: List[String]): String = {
    val adapters = inputs.map(_.toInt).sorted
    val fullChain = 0 :: (adapters :+ adapters.last+3)
    val diffs = fullChain.sliding(2).map(x => x(1) - x(0)).toList

    (diffs.count(_ == 1) * diffs.count(_ == 3)).toString
  }

  def solve2(inputs: List[String]): String = {
    val adapters = inputs.map(_.toInt).toArray.sorted
    val fullChain = 0 +: adapters :+ adapters.last+3

    val memoize = Array.fill(fullChain.size)(None: Option[Long])
    memoize(fullChain.size-1) = Some(1)

    def countWays(from: Int): Long = {
      memoize(from).getOrElse {
        val currVal = fullChain(from)
          val ways = (for {
            i <- from+1 to math.min(from+3, fullChain.size-1)
            if fullChain(i) - currVal <= 3
          } yield countWays(i)).sum
          
          memoize(from) = Some(ways)

          ways
      }
    }

    countWays(0).toString
  }
}