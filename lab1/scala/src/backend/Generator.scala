package backend

/**
 * Code generator for pseudo assembly. Implements
 * a "convenient munch" algorithm.
 *
 * @author Robin Steiger
 * @revision Andre Platzer
 * @revision Ian Gillis
 */
object Generator {
  import scala.collection.mutable.HashMap
  import frontend.Trees
  import Trees._
  import backend.Assembly
  import Assembly._
  import util.TempFactory
  import TempFactory.Temp

  // map from a variable to the temp storing its current value
  var env = new HashMap[Ident,Temp]()

  // should never get thrown
  class UninitializedVariableException(m : String) extends Exception

  // generate assembly instruction directly from the AST
  def generate(program: Trees.Program): Assembly.Program = {
    new Assembly.Program(munchProgram(program))
  } 

  private def munchProgram(program : Trees.Program) = program.mainFunction match {
      /*
       * undeclared variables or improper definition of main 
       * should be handled in semantic analysis
       */
      case Function(_, _, body) => (body map munchStatement).flatten
  }

  /*
   * generates a list of instructions to execute stm
   */
  private def munchStatement(stm : Statement) = stm match {
    case Assign(id, exp) =>
      val t = TempFactory.newTemp
      env.put(id,t)
      munchExp(Assembly.Temp(t), exp)
    case Return(exp) => munchExp(Register(Eax), exp)
  }

  /*
   * generates a list of instructions to achieve dest <- exp
   * d must be a Temp(t) or a Register(reg)
   */
  private def munchExp(dest : Operand, exp : Expression) : List[Instruction] = exp match {
    case IntLit(i) => List(Move(dest,Immediate(i)))
    case Ident(s) => env.get(Ident(s)) match {
      case Some(t) => List(Move(dest,Temp(t)))
      // should be handled in semantic analysis
      case None => throw new UninitializedVariableException(s)
    }
    case Trees.BinOp(left, op, right) => munchBinOp(dest, left, op, right)
    // turn -x into 0-x
    case UnOp(Negate, exp) => munchBinOp(dest, IntLit(0), Trees.Sub, exp)
    // the only supported unary operator is negation
    case UnOp(op,_) => throw new UnsupportedOperationException(op.toString)
  }

  /*
   * generates a list of instructions to achieve dest <- left op right
   * d must be a Temp(t) or a Register(reg)
   */
  private def munchBinOp(dest : Operand, left : Expression, 
      op : Primitive, right : Expression) : List[Instruction] = { 
    val t1 = Temp(TempFactory.newTemp)
    val t2 = Temp(TempFactory.newTemp)
    val operation = munchOp(op)
    munchExp(t1, left) ::: munchExp(t2, right) ::: 
      List(Assembly.BinOp(operation, dest, t1, t2))
  }

  private def munchOp(op : Primitive) = op match {
    case Trees.Add => Assembly.Add
    case Trees.Sub => Assembly.Sub
    case Trees.Mul => Assembly.Mul
    case Trees.Div => Assembly.Div
    case Trees.Mod => Assembly.Mod
    // for completeness, handle negation
    case Trees.Negate => Assembly.Sub
  }
}
