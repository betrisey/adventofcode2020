object Day23 {
  def solve(inputs: List[String]): String = {
    case class Node(val value: Int, var next: Node)

    def find(head: Node, value: Int): Node = {
      if head.value == value then head
      else find(head.next, value)
    }

    val input = inputs.head.toCharArray.map(_.asDigit)
    val N = input.size
    val nodes = input.map(Node(_, null))
    for (i <- 0 until N) {
      nodes(i).next = nodes((i+1)%N)
    }
      
    var head = nodes.head
    
    for (_ <- 0 until 100) {
      val removed = head.next
      val removedList = removed.value :: removed.next.value :: removed.next.next.value :: Nil
      head.next = head.next.next.next.next
  
      val destValue = (head.value-1 to head.value-4 by -1).map(x => math.floorMod(x-1, N)+1)
        .find(!removedList.contains(_)).get
  
      val dest = find(head, destValue)
      removed.next.next.next = dest.next
      dest.next = removed

      head = head.next
    }

    head = find(head, 1)
    val answer = scala.collection.mutable.ListBuffer.empty[Int]
    for (_ <- 0 until N-1) {
      head = head.next
      answer += head.value
    }
    answer.toList.mkString
  }

  def solve2(inputs: List[String]): String = {
    import scala.collection.mutable.Map
    val input = inputs.head.toCharArray.map(_.asDigit)
    val completeInput = input ++ (input.size + 1 to 1000000)
    var map = Map(completeInput.zip(completeInput.tail :+ completeInput.head): _*)
    val N = completeInput.size
    
    var head = completeInput.head

    for (i <- 0 until 10000000) {
      if i%1000000==0 then println(s"$i/10000000")

      val removed1 = map(head)
      val removed2 = map(removed1)
      val removed3 = map(removed2)
      val removedList = removed1 :: removed2 :: removed3 :: Nil
      map(head) = map(removed3)
  
      val dest = (head-1 to head-4 by -1).map(x => math.floorMod(x-1, N)+1)
        .find(!removedList.contains(_)).get
  
      map(removed3) = map(dest)
      map(dest) = removed1

      head = map(head)
    }

    (map(1).toLong*map(map(1)).toLong).toString
  }
}
