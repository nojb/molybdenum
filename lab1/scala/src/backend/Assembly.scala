package backend

/**
 * L1 Compiler
 * Assembly language
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 * We write
 * 
 * BINOP  operand1 <- operand2,operand3
 * MOV    operand1 <- operand2
 *
 * @author Ian Gillis (igillis@andrew.cmu.edu)
 * @revision Sri Raghavan (srikrish@andrew.cmu.edu)
 */
 
object Assembly {
  import util.TempFactory.{Temp => TempVal}
  
  class Program(instrs : List[Instruction]) {
    override def toString : String = 
      (instrs map (instr => instr.toString)).foldLeft ("") (_+_) 
  }
 
  // Registers
  sealed abstract class Reg(val name : String) {
    override def toString : String = name
  }
  case object Eax extends Reg("eax")

  // Data type for different operands
  sealed abstract class Operand(val value : String) {
    override def toString : String = value;
  }
  case class Immediate(imm : Int) extends Operand("$" + imm)
  case class Register(reg : Reg) extends Operand(reg.name)
  case class Temp(temp : TempVal) extends Operand(temp.toString)

  // Data type for the L1 operations
  sealed abstract class Operation(val op : String) {
    override def toString: String = op
  }
  case object Add extends Operation("add")
  case object Sub extends Operation("sub")
  case object Mul extends Operation("mul")
  case object Div extends Operation("div")
  case object Mod extends Operation("mod")

  // Instructions for L1
  sealed trait Instruction {
    override def toString : String = PrettyPrinter(this);
  }
  case class BinOp(op: Operation, dest : Operand, v2 : Operand, 
    v3 : Operand) extends Instruction
  case class Move(dest : Operand, src : Operand) extends Instruction
  case class Directive(s : String) extends Instruction
  case class Comment(s : String) extends Instruction

  object PrettyPrinter {
    def apply(instr : Instruction) = pp(instr)

    //Helper methods
    private def pp(instr : Instruction) : String = instr match {
      case BinOp(op, v1, v2, v3) => 
        "\t" + op.toString + "\t" + v1.toString + 
        " <- " + v2.toString + "," + v3.toString + "\n"
      case Move(dest,src) => 
        "\t" + "mov" + "\t" + dest.toString + " <- " + src.toString + "\n"
      case Directive(s) => "\t" + s + "\n"
      case Comment(s) => "\t" + "/* " + s + " */\n"
    }
  }
}
