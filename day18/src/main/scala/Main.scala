import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("inputs/input-18.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day18.solve2(input)
    println(output)
  }

}
