object Day8 {
  sealed trait Instruction
  case class Nop(i: Int) extends Instruction
  case class Acc(i: Int) extends Instruction
  case class Jmp(i: Int) extends Instruction

  def parseInstruction(input: String): Instruction = {
    val Array(op, imm) = input.split(" ")
    (op, imm.toInt) match
      case ("nop", i) => Nop(i)
      case ("acc", i) => Acc(i)
      case ("jmp", i) => Jmp(i)
  }
  
  def solve(inputs: List[String]): String = {
    val instructions = inputs.map(parseInstruction).toArray
    
    def execute(pc: Int = 0, acc: Int = 0, executed: Set[Int] = Set.empty): Int = {
      if executed.contains(pc) then acc
      else
        instructions(pc) match
          case Nop(_) => execute(pc+1, acc, executed + pc)
          case Acc(i) => execute(pc+1, acc+i, executed + pc)
          case Jmp(i) => execute(pc+i, acc, executed + pc)
    }

    execute().toString
  }

  def solve2(inputs: List[String]): String = {
    val instructions = inputs.map(parseInstruction).toArray

    def changeInstruction(instr: Instruction): Instruction = {
      instr match
        case Nop(i) => Jmp(i)
        case Jmp(i) => Nop(i)
        case instr => instr
    }

    def tryExecute(changeIndex: Int, pc: Int = 0, acc: Int = 0, executed: Set[Int] = Set.empty): Option[Int] = {
      if pc > instructions.size then None // Error
      else if pc == instructions.size then Some(acc) // Terminates
      else if executed.contains(pc) then None // Loop
      else
        val instr = if pc == changeIndex then changeInstruction(instructions(pc)) else instructions(pc)
        instr match
          case Nop(_) => tryExecute(changeIndex, pc+1, acc, executed + pc)
          case Acc(i) => tryExecute(changeIndex, pc+1, acc+i, executed + pc)
          case Jmp(i) => tryExecute(changeIndex, pc+i, acc, executed + pc)
    }

    (for {
      changeIndex <- 0 until instructions.size
      if instructions(changeIndex) match
        case Nop(_) | Jmp(_) => true
        case _ => false
      output <- tryExecute(changeIndex)
    } yield output).head.toString
  }
}