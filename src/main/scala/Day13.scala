object Day13 {
  def solve(inputs: List[String]): String = {
    val start = inputs(0).toInt
    val busses = inputs(1).split(",").filter(_ != "x").map(_.toInt)
    val minBus = busses.map(id => (id, if start%id==0 then 0 else id - start%id))
      .minBy(_._2)
    (minBus._1 * minBus._2).toString
  }

  def solve2(inputs: List[String]): String = {
    val busses = inputs(1).split(",").zipWithIndex.filter(_._1 != "x").map(x => (x._1.toInt, x._2))
    val contraints = busses.map(bus => s"(x + ${bus._2}) mod ${bus._1} = 0").mkString(", ")
    // TODO Solve using chinese remainder theorem
    "Input into WolframAlpha:\n" + contraints
  }
}