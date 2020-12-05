object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val source = Source.fromFile("input-5.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day5.solve2(input);
    println(output);
  }

}
