import scala.collection.mutable.TreeMap
object Day14 {

  def applyMask(number: String, mask: String): Long = {
    val padding = Math.max(mask.length - number.length, 0)
    val binaryOutput = ("0"*padding + number).zip(mask).map {
      case (bitNumber, bitMask) =>
        bitMask match
          case 'X' => bitNumber
          case other => other
    }.mkString
    BigInt(binaryOutput, 2).toLong
  }

  def solve(inputs: List[String]): String = {
    val mem = TreeMap.empty[Long, Long]
    var mask = ""
    for (input <- inputs) {
      if (input.startsWith("mask = ")) {
        mask = input.drop("mask = ".length)
      } else {
        val format = raw"mem\[(\d+)\] = (\d+)".r
        input match {
          case format(addr, value) =>
            mem(addr.toLong) = applyMask(value.toLong.toBinaryString, mask)
        }
      }
    }
    mem.values.sum.toString
  }

  def applyMask2(number: String, mask: String): List[Long] = {
    val padding = Math.max(mask.length - number.length, 0)
    val binaryOutput = ("0"*padding + number).zip(mask).map {
      case (bitNumber, bitMask) =>
        bitMask match
          case '0' => bitNumber
          case '1' => '1'
          case 'X' => 'X'
          case other => other
    }.toList

    def generate(list: List[Char]): List[List[Char]] = {
      list match
        case Nil => List(Nil)
        case 'X' :: tail => generate(tail).map('0' :: _) ++
                            generate(tail).map('1' :: _)
        case head :: tail => generate(tail).map(head :: _)
    }

    generate(binaryOutput).map(b => BigInt(b.mkString, 2).toLong)
  }

  def solve2(inputs: List[String]): String = {
    val mem = TreeMap.empty[Long, Long]
    var mask = ""
    for (input <- inputs) {
      if (input.startsWith("mask = ")) {
        mask = input.drop("mask = ".length)
      } else {
        val format = raw"mem\[(\d+)\] = (\d+)".r
        input match {
          case format(addr, value) =>
            for (decodedAddr <- applyMask2(addr.toLong.toBinaryString, mask)) {
              mem(decodedAddr) = value.toLong
            }
        }
      }
    }
    mem.values.sum.toString
  }
}