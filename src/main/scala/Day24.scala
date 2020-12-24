object Day24 {
  def toVector(dir: String): (Int, Int) = {
    dir match
      case "e" => (1,0)
      case "ne" => (0,1)
      case "nw" => (-1,1)
      case "w" => (-1,0)
      case "sw" => (0,-1)
      case "se" => (1,-1)
  }

  def getBlackTiles(inputs: List[String]): List[(Int, Int)] = {
    val tiles = inputs.map(_.split(",").map(toVector).reduce((x,y) => (x._1+y._1, x._2+y._2)))
    tiles.groupBy(x => x).filter(_._2.size % 2 == 1).keys.toList
  }

  def solve(inputs: List[String]): String = {
    getBlackTiles(inputs).size.toString
  }

  def solve2(inputs: List[String]): String = {
    val blackTiles = getBlackTiles(inputs)
    
    val grid = Array.fill(300, 300)(false)

    val offset = 150
    for ((i,j) <- blackTiles.map(x => (x._1+offset, x._2+offset))) {
      grid(i)(j) = true
    }

    def countNeighboor(current: Array[Array[Boolean]], i: Int, j: Int): Int = {
      val dirs = List((1,0), (0,1), (-1,1), (-1,0), (0,-1), (1,-1))
      dirs.map(dir => (i+dir._1, j+dir._2))
        .filter(pos => pos._1 >= 0 && pos._1 < 300 && pos._2 >= 0 && pos._2 < 300)
        .map(pos => current(pos._1)(pos._2))
        .count(_ == true)
    }

    def nextState(current: Array[Array[Boolean]]): Array[Array[Boolean]] = {
      current.zipWithIndex.map {
        case (row, i) => row.zipWithIndex.map {
          case (cell, j) => (cell, countNeighboor(current, i, j)) match {
            case (true, count) if count == 0 || count > 2 => false
            case (false, count) if count == 2 => true
            case _ => cell
          }
        }
      }
    }

    def applyN(current: Array[Array[Boolean]], N: Int): Array[Array[Boolean]] = {
      if N == 0 then current
      else
        applyN(nextState(current), N-1)
    }

    applyN(grid, 100).map(_.count(_ == true)).sum.toString
  }
}
