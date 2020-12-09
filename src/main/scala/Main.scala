object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val source = Source.fromFile("input-8.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day8.solve2(input);
    println(output);
  }

}
