object Day16 {
  def parseField(field: String): List[Int] = {
    val format = raw".*: (\d+)-(\d+) or (\d+)-(\d+)".r
      field match
        case format(start1, end1, start2, end2) =>
          ((start1.toInt to end1.toInt) union (start2.toInt to end2.toInt)).distinct.toList
  }

  def solve(inputs: List[String]): String = {
    val fields = inputs.takeWhile(_ != "").map(parseField)
    val otherTickets = inputs.drop(fields.size + 5).map(_.split(",").map(_.toInt))
    
    val allowedValues = fields.flatten.distinct

    val numbers = otherTickets.flatten
    numbers.filter(!allowedValues.contains(_)).sum.toString
  }

  def solve2(inputs: List[String]): String = {
    val fields = inputs.takeWhile(_ != "").map(parseField)
    val myTicket = inputs.drop(fields.size + 2).head.split(",").map(_.toInt)
    val otherTickets = inputs.drop(fields.size + 5).map(_.split(",").map(_.toInt))
    
    val allowedValues = fields.flatten.distinct

    val validTickets = myTicket :: otherTickets.filter(_.forall(allowedValues.contains))

    val possibleFieldForColumns = validTickets.transpose
      .map(col => fields.zipWithIndex.filter(field => col.forall(value => field._1.contains(value))).map(_._2).toSet)
      .zipWithIndex
      .sortBy(_._1.size)
    possibleFieldForColumns.toString

    println(possibleFieldForColumns)

    val used = possibleFieldForColumns.scanLeft(Set[Int]())((list, current) => list ++ current._1).init

    val mapping = possibleFieldForColumns.zip(used)
      .map(x => (x._1._2, (x._1._1 diff x._2).head))
      .sortBy(_._2)

    mapping.take(6).map(x => myTicket(x._1).toLong).product.toString
  }
}