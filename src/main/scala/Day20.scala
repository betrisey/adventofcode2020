import scala.reflect.ClassTag

object Day20 {
  case class Tile[T:ClassTag](id: Int, values: Array[Array[T]]) {
    def borders: List[List[T]] =
      // Top, left, bottom, right
      List(values.head, values.transpose.head, values.last, values.transpose.last).map(_.toList)
    
    def rotate(input: Array[Array[T]]): Array[Array[T]] = {
      val rotated = input.zipWithIndex.map {
        case (row, i) => row.zipWithIndex.map {
          case (_, j) => input(j)(input.size-1-i)
        }
      }
      rotated
    }

    def rotateN(N: Int, input: Array[Array[T]] = values):  Array[Array[T]] = {
      val Nmod = Math.floorMod(N, 4)
      if Nmod == 0 then input
      else rotateN(N-1, rotate(input))
    }

    def flip(input: Array[Array[T]] = values): Array[Array[T]] = input.reverse

    def rotateFlipTile(N: Int, flip: Boolean): Tile[T] = {
      Tile(id, rotateN(N, if flip then this.flip() else values))
    }
  }

  def parseTiles(inputs: List[String]): List[Tile[Boolean]] = {
    if inputs == Nil then Nil
    else
      val (tile, "" :: rest) = inputs.span(_ != "")
      
      Tile(tile.head.drop("Tile ".size).init.toInt,
           tile.tail.map(_.toCharArray.map(_ == '#')).toArray)
      :: parseTiles(rest)
  }

  def solve(inputs: List[String]): String = {
    val tiles = parseTiles(inputs)

    def countUniqueBorders(tile: Tile[Boolean]): Int = {
      tile.borders.count(border =>
        tiles.filter(_ != tile)
          .forall(_.borders.forall(border2 => border != border2 && border.reverse != border2)))
    }

    val borderUniqueCounts = tiles.map(tile => (tile, countUniqueBorders(tile)))
    
    borderUniqueCounts
      .filter(_._2 == 2)
      .map(_._1.id.toLong)
      .product.toString
  }

  def solve2(inputs: List[String]): String = {
    val tiles = parseTiles(inputs)

    def findUniqueBorders(tile: Tile[Boolean]): List[Int] = {
      tile.borders.zipWithIndex.filter(border =>
        tiles.filter(_ != tile)
          .forall(_.borders.forall(border2 => border._1 != border2 && border._1.reverse != border2)))
        .map(_._2)
    }

    val uniqueBordersPerTile = tiles.map(tile => (tile, findUniqueBorders(tile)))
    val corners = uniqueBordersPerTile
      .filter(_._2.size == 2)
    
    val solution = Array.fill(12, 12)(None: Option[Tile[Boolean]])
    var tilesLeft = tiles.toSet
    solution(0)(0) = Some(corners.head._1.rotateFlipTile(-corners.head._2.head, false))
    tilesLeft -= corners.head._1

    def findMatch(current: Tile[Boolean], dir: (Int, Int), pos: (Int, Int)): Tile[Boolean] = {
      val borderIndex = dir match
        case (0,1) => 2
        case (-1,0) => 1
        case (0,-1) => 0
        case (1,0) => 3
      val border = current.borders(borderIndex)

      val borderIndex2 = dir match
        case (0,1) => 0
        case (-1,0) => 3
        case (0,-1) => 2
        case (1,0) => 1
      
      val matches = for {
        tile <- tilesLeft
        flip <- List(false, true)
        rotation <- 0 until 4
        newTile = tile.rotateFlipTile(rotation, flip)
        if newTile.borders(borderIndex2) == border
      } yield (tile, newTile)
      
      val (tile, newTile) = matches.head

      tilesLeft -= tile
      newTile
    }

    def isPosFree(pos: (Int, Int)): Boolean = {
      pos._1 >= 0 && pos._1 < 12 && pos._2 >= 0 && pos._2 < 12 &&
        solution(pos._2)(pos._1).isEmpty
    }

    def nextDir(dir: (Int, Int), pos: (Int, Int)): Option[(Int, Int)] = {
      if isPosFree((pos._1 + dir._1, pos._2 + dir._2)) then Some(dir)
      else
        val newDir = dir match
          case (1,0) => (0,-1)
          case (0,-1) => (-1,0)
          case (-1,0) => (0,1)
          case (0,1) => (1,0)
        if isPosFree((pos._1 + newDir._1, pos._2 + newDir._2)) then Some(newDir)
        else None
    }

    var dir = nextDir((0,1), (0,0))
    var pos = (0,0)
    while(dir.isDefined) {
      val newPos = (pos._1 + dir.get._1, pos._2 + dir.get._2)
      solution(newPos._2)(newPos._1) = Some(findMatch(solution(pos._2)(pos._1).get, dir.get, pos))
      pos = newPos
      dir = nextDir(dir.get, pos)
    }
    
    def notBorder(i: Int): Boolean = {
      val imod = i % 10
      imod != 0 && imod != 9
    }
    val grid = (0 until 12*10).filter(notBorder).map(i => (0 until 12*10).filter(notBorder)
      .map(j => if solution(i/10)(j/10).get.values(i%10)(j%10) then '#' else '.').toArray).toArray
   
    val gridTile = Tile(0, grid)
    
    val monster = List("                  # ",
                       "#    ##    ##    ###",
                       " #  #  #  #  #  #   ")
    val monsterCoords = monster.map(_.toCharArray)
      .zipWithIndex.flatMap { case (row, i) =>
        row.zipWithIndex.flatMap { case (cell, j) =>
          if cell == '#' then Some((i,j)) else None
        }
      }
    
    (for {
      flip <- List(false, true)
      rotation <- 0 until 4
      newGrid = gridTile.rotateFlipTile(rotation, flip).values
    } yield {
      for {
        i <- 0 to newGrid.length - monster.length
        j <- 0 to newGrid.length - monster.head.length
      } {
        val coords = monsterCoords.map(coord => (coord._1 + i, coord._2 + j))
        if coords.forall(coord => newGrid(coord._1)(coord._2) != '.') then
          coords.foreach(coord => newGrid(coord._1)(coord._2) = 'O')
      }
      newGrid.map(_.count(_ == '#')).sum
    }).min.toString
  }
}
