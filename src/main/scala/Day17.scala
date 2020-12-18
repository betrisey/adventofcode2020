object Day17 {
  def solve(inputs: List[String]): String = {
    type State = Array[Array[Array[Boolean]]]
    val cycles = 6

    val space = Array.fill(1 + 2*cycles, inputs.size + 2*cycles, inputs.head.size + 2*cycles)(false)

    for (i <- 0 until inputs.size; j <- 0 until inputs.head.size) {
      space(cycles)(cycles + i)(cycles + j) = inputs(i)(j) == '#'
    }

    def countNeighboor(state: State, i: Int, j: Int, k: Int): Int = {
      (for {
        x <- i-1 to i+1
        y <- j-1 to j+1
        z <- k-1 to k+1
        if !(x == i && y == j && z == k)
        if (x >= 0 && y >= 0 && z >= 0 &&
          x < state.length && y < state(0).length && z < state(0)(0).length)
        if state(x)(y)(z)
      } yield 1).sum
    }

    def applyN(current: State, N: Int): State = {
      if N == 0 then current
      else
        applyN(current.zipWithIndex.map {
          case (plane, i) => plane.zipWithIndex.map {
            case (row, j) => row.zipWithIndex.map {
              case (cell, k) =>
                val count = countNeighboor(current, i, j, k)
                if cell then
                  count == 2 || count == 3
                else
                  count == 3
            }
          }
        }, N-1)
    }

    val output = applyN(space, cycles)
    output.flatten.flatten.count(_ == true).toString
  }

  def solve2(inputs: List[String]): String = {
    type State = Array[Array[Array[Array[Boolean]]]]
    val cycles = 6

    val space = Array.fill(1 + 2*cycles, 1 + 2*cycles, inputs.size + 2*cycles, inputs.head.size + 2*cycles)(false)

    for (i <- 0 until inputs.size; j <- 0 until inputs.head.size) {
      space(cycles)(cycles)(cycles + i)(cycles + j) = inputs(i)(j) == '#'
    }

    def countNeighboor(state: State, i: Int, j: Int, k: Int, l: Int): Int = {
      (for {
        x <- i-1 to i+1
        y <- j-1 to j+1
        z <- k-1 to k+1
        t <- l-1 to l+1
        if !(x == i && y == j && z == k && t == l)
        if (x >= 0 && y >= 0 && z >= 0 && t >= 0 &&
          x < state.length && y < state(0).length && z < state(0)(0).length && t < state(0)(0)(0).length)
        if state(x)(y)(z)(t)
      } yield 1).sum
    }

    def applyN(current: State, N: Int): State = {
      if N == 0 then current
      else
        applyN(current.zipWithIndex.map {
          case (dim4, i) => dim4.zipWithIndex.map {
            case (plane, j) => plane.zipWithIndex.map {
              case (row, k) => row.zipWithIndex.map {
                case (cell, l) =>
                  val count = countNeighboor(current, i, j, k, l)
                  if cell then
                    count == 2 || count == 3
                  else
                    count == 3
              }
            }
          }
        }, N-1)
    }

    val output = applyN(space, cycles)
    output.flatten.flatten.flatten.count(_ == true).toString
  }
}