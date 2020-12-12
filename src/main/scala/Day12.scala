object Day12 {
  type Position = (Int,Int)
  type State = (Position,Char)

  def move(dir: Char, length: Int, lastPos: Position): Position = {
    val dirTupple = dir match {
      case 'N' => (-1, 0)
      case 'S' => (1, 0)
      case 'E' => (0, 1)
      case 'W' => (0, -1)
    }
    (lastPos._1 + dirTupple._1 * length, lastPos._2 + dirTupple._2 * length)
  }

  val directions = Array('E', 'N', 'W', 'S')
  def rotate(dir: Char, angle: Int, lastState: State): State = {
    val newDir = directions(Math.floorMod(directions.indexOf(lastState._2) + angle/90, directions.size))
    (lastState._1, newDir)
  } 
  
  def solve(inputs: List[String]): String = {
    val finalPos = inputs.foldLeft(((0,0),'E'))(((lastState, input) => (input.head, input.tail.toInt) match
      case ('F', n) => (move(lastState._2, n, lastState._1), lastState._2)
      case ('L', n) => rotate('L', n, lastState)
      case ('R', n) => rotate('R', -n, lastState)
      case (dir, n) => (move(dir, n, lastState._1), lastState._2)
    ))
    (finalPos._1._1.abs + finalPos._1._2.abs).toString
  }

  def rotateVector(vector: Position, angle: Int): Position = {
    Math.floorMod(angle, 360) match
      case 0 => vector
      case 90 => (-vector._2, vector._1)
      case 180 => (-vector._1, -vector._2)
      case 270 => (vector._2, -vector._1)
  }

  def solve2(inputs: List[String]): String = {
    val finalPos = inputs.foldLeft((0,0),(-1,10))(((lastState, input) => (input.head, input.tail.toInt) match
      case ('F', n) => ((lastState._1._1 + n*lastState._2._1, lastState._1._2 + n*lastState._2._2), lastState._2)
      case ('L', n) => (lastState._1, rotateVector(lastState._2, n))
      case ('R', n) => (lastState._1, rotateVector(lastState._2, -n))
      case (dir, n) => (lastState._1, move(dir, n, lastState._2))
    ))
    (finalPos._1._1.abs + finalPos._1._2.abs).toString
  }
}