object Day22 {
  import scala.collection.immutable.Queue

  type Decks = (Queue[Int],Queue[Int])

  def parseDecks(inputs: List[String]): (Queue[Int], Queue[Int]) = {
    val players = inputs.span(_ != "").toList.map(x =>
      Queue(x.filter(_ != "").toList.tail.map(_.toInt): _*))
    
    (players(0), players(1))
  }

  def playUntilEnd(decks: Decks, playTurn: (Decks, Set[Decks]) => (Decks, Set[Decks]), seenConfigs: Set[Decks]): (Queue[Int], Int) = {
    if decks._1.isEmpty then (decks._2, 2)
    else if decks._2.isEmpty then (decks._1, 1)
    else
      val (newDecks, newSeenConfigs) = playTurn(decks, seenConfigs)
      playUntilEnd(newDecks, playTurn, newSeenConfigs)
  }

  def computeScore(winningDeck: Queue[Int]): Int =
    (0 :: winningDeck.toList.reverse).zipWithIndex.map(x => x._1 * x._2).sum

  def solve(inputs: List[String]): String = {
    val decks = parseDecks(inputs)

    def playTurn(decks: Decks, seenConfigs: Set[Decks]): (Decks, Set[Decks]) = {
      if decks._1.head > decks._2.head then
        ((decks._1.tail :+ decks._1.head :+ decks._2.head, decks._2.tail), Set.empty)
      else
        ((decks._1.tail, decks._2.tail :+ decks._2.head :+ decks._1.head), Set.empty)
    }

    val winningDeck = playUntilEnd(decks, playTurn, Set.empty)._1
    computeScore(winningDeck).toString
  }

  def solve2(inputs: List[String]): String = {
    val decks = parseDecks(inputs)

    def playTurn(decks: Decks, seenConfigs: Set[Decks]): (Decks, Set[Decks]) = {
      if seenConfigs.contains(decks) then ((decks._1, Queue.empty), seenConfigs)
      else
        val head1 = decks._1.head
        val tail1 = decks._1.tail
        val head2 = decks._2.head
        val tail2 = decks._2.tail

        val winner =
          if tail1.size >= head1 && tail2.size >= head2 then
            playUntilEnd((tail1.take(head1), tail2.take(head2)), playTurn, seenConfigs)._2
          else
            if head1 > head2 then 1 else 2
        
        val newSeenConfigs = seenConfigs + decks
        if winner == 1 then
          ((tail1 :+ head1 :+ head2, tail2), newSeenConfigs)
        else
          ((tail1, tail2 :+ head2 :+ head1), newSeenConfigs)
    }

    val winningDeck = playUntilEnd(decks, playTurn, Set.empty)._1
    computeScore(winningDeck).toString
  }
}
