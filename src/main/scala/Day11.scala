object Day11 {
  val floor = '.'
  val empty = 'L'
  val occupied = '#'

  type State = Array[Array[Char]]

  def parseInput(inputs: List[String]): State = {
    inputs.map(_.toCharArray).toArray
  }

  def nextState(current: State, countNeighboor: (State, Int, Int) => Int, threshold: Int): (State, Boolean) = {
    var changed = false

    val next = current.zipWithIndex.map {
      case (row, i) => row.zipWithIndex.map {
        case (cell, j) => (cell, countNeighboor(current, i, j)) match {
          case ('L', 0) => changed = true; occupied
          case ('#', count) if count >= threshold => changed = true; empty
          case _ => cell
        }
      }
    }

    (next, changed)
  }

  def applyUntilStable(current: State, countNeighboor: (State, Int, Int) => Int, threshold: Int): State = {
    //println(current.map(_.mkString(" ")).mkString("\n") + "\n")
    val (next, changed) = nextState(current, countNeighboor, threshold)
    if changed then applyUntilStable(next, countNeighboor, threshold)
    else current
  }

  def solve(inputs: List[String]): String = {
    def countNeighboor(state: State, i: Int, j: Int): Int = {
      (for {
        x <- i-1 to i+1
        y <- j-1 to j+1
        if !(x == i && y == j)
        if (x >= 0 && y >= 0 && x < state.length && y < state(0).length)
        if state(x)(y) == occupied
      } yield 1).sum
    }

    applyUntilStable(parseInput(inputs), countNeighboor, 4)
      .map(_.count(_ == '#'))
      .sum.toString
  }

  def solve2(inputs: List[String]): String = {
    def countNeighboor(state: State, i: Int, j: Int): Int = {
      def occupiedInDir(i: Int, j: Int, dirI: Int, dirJ: Int): Boolean = {
        (for {
          n <- 1 until state.size
          x = i + n*dirI
          y = j + n*dirJ
          if (x >= 0 && y >= 0 && x < state.length && y < state(0).length)
          cell = state(x)(y)
          if cell != floor
        } yield cell).headOption.map(_ == occupied).getOrElse(false)
      }

      (for {
        dirI <- -1 to 1
        dirJ <- -1 to 1
        if !(dirI == 0 && dirJ == 0)
        if occupiedInDir(i, j, dirI, dirJ)
      } yield 1).sum
    }

    applyUntilStable(parseInput(inputs), countNeighboor, 5)
      .map(_.count(_ == '#'))
      .sum.toString
  }
}