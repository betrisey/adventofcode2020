object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val source = Source.fromFile("inputs/input-18.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day18.solve(input);
    println(output);
  }

}
