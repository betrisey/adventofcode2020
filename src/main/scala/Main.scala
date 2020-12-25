object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val source = Source.fromFile("inputs/input-25.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day25.solve(input)
    println(output)
  }

}
