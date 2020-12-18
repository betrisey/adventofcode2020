import scala.collection.mutable.TreeMap
object Day15 {
  def getNthNumber(inputs: List[String], index: Int): String = {
    val spokenNumbers = TreeMap.empty[Int, Int]
    val input = inputs.head.split(",").map(_.toInt)
    spokenNumbers.addAll(input.init.zipWithIndex)

    var lastNumber = input.last
    for (turn <- input.size until index) {
      val newNumber = turn-1 - spokenNumbers.getOrElseUpdate(lastNumber, turn-1)
      spokenNumbers.update(lastNumber, turn-1)
      lastNumber = newNumber
    }
    
    lastNumber.toString
  }

  def solve(inputs: List[String]): String = {
    getNthNumber(inputs, 2020)
  }

  def solve2(inputs: List[String]): String = {
    getNthNumber(inputs, 30000000)
  }
}