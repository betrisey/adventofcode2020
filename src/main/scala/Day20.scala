object Day20 {
  case class Tile(id: Int, values: Array[Array[Boolean]]) {
    val borders: List[List[Boolean]] =
      List(values.head, values.last, values.transpose.head, values.transpose.last).map(_.toList)
    
    def print(): Unit = {
      println(id)
      println(borders.map(_.map(if _ then '#' else '.').mkString).mkString("\n"))
    }
  }

  def parseTiles(inputs: List[String]): List[Tile] = {
    if inputs == Nil then Nil
    else
      val (tile, "" :: rest) = inputs.span(_ != "")
      
      Tile(tile.head.drop("Tile ".size).init.toInt,
           tile.tail.map(_.toCharArray.map(_ == '#')).toArray)
      :: parseTiles(rest)
  }

  def solve(inputs: List[String]): String = {
    val tiles = parseTiles(inputs)

    def countUniqueBorders(tile: Tile): Int = {
      tile.borders.count(border =>
        tiles.filter(_ != tile)
          .forall(_.borders.forall(border2 => border != border2 && border.reverse != border2)))
    }

    val borderUniqueCounts = tiles.map(tile => (tile, countUniqueBorders(tile)))
    //println(borderUniqueCounts.sortBy(-_._2))
    borderUniqueCounts
      .filter(_._2 == 2)
      .map(_._1.id.toLong)
      .product.toString
    
    /*val possibleTiles = for {
      tile1 <- tiles
      b1 <- tile1.borders
      tile2 <- tiles
      b2 <- tile2.borders
      if b1 == b2 || b1 == b2.reverse
    } yield (tile1, tile2)
    val x = possibleTiles.groupBy(_._1).mapValues(_.size).toMap
    println(x)*/
  }

  def solve2(inputs: List[String]): String = {
    ""
  }
}
