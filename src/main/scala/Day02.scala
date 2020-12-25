object Day02 {
  def solve(inputs: List[String]): String = {
    val format = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
    def isValid(input: String): Boolean = {
      input match {
        case format(min, max, letter, password) =>
          val char = letter.toCharArray.head
          val count = password.count(_ == char)
          count >= min.toInt && count <= max.toInt
      }
    }

    inputs.count(isValid).toString
  }

  def solve2(inputs: List[String]): String = {
    val format = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
    def isValid(input: String): Boolean = {
      input match {
        case format(i, j, letter, password) =>
          val char = letter.toCharArray.head
          (password(i.toInt-1) == char) != (password(j.toInt-1) == char)
      }
    }

    inputs.count(isValid).toString
  }
}