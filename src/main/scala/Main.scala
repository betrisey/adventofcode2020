object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val source = Source.fromFile("inputs/input-10.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day10.solve2(input);
    println(output);
  }

}
