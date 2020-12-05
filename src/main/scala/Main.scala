object Main {

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val source = Source.fromFile("input-4.txt")
    val input = source.getLines().toList
    source.close()
    
    val output = Day4.solve2(input);
    println(output);
  }

}
