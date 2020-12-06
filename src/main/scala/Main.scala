object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val source = Source.fromFile("input-6.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day6.solve2(input);
    println(output);
  }

}
