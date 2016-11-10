package top

/**
 *  Analyze programs and verify the static semantics.
 * @author Robin Steiger
 * @revision Andre Platzer
 */
object Analyzer {
  import scala.collection.immutable.Set
  import frontend.Trees._
  
  /** Verifies the static semantics of the program.
   *
   *  @param program	the program to analyse
   *  @return   	the program, transformed
   */
  def verifyProgram(program: Program): Unit = {
    mainFunction(program)
    duplicateDefinition(program.mainFunction)
    nameAnalysis(program.mainFunction)
    initializedVariables(program.mainFunction)
  }
  
  private def duplicateDefinition(function: Function) {
    def findDuplicates(vars: List[Ident]) : Unit = vars match {
      case Nil =>
      case v :: vs if (vs contains v) =>
        Report.error (v.pos, "Duplicate variable declaration " + v.name)
      case _ :: vs =>
        findDuplicates(vs)
    }
    findDuplicates(function.decls)
  }

  private def mainFunction(program: Program) {
    val functions = List(program.mainFunction)
    if (!(functions exists {_.id.name == "main"})) {
      Report error "There is no 'main' function."
    }
  }

  private def nameAnalysis(function: Function) {
    val vars: List[Ident] = function.decls
    def nameAnalysisBody(body: Body) {
      body foreach nameAnalysisStatement
    }
    def nameAnalysisStatement(stmt: Statement) : Unit = stmt match {
      case Assign(id, exp) =>
        if (!(vars contains id)) {
          Report.error (stmt.pos, "Undeclared variable")
        } else {
            nameAnalysis(exp)
        }
      case Return(exp) =>
        nameAnalysis(exp)
    }
    def nameAnalysis(expr: Expression) : Unit = expr match {
      case id@Ident(_) =>
        if (!(vars contains id)) {
          Report.error (expr.pos, "Undeclared variable")
        }
      case UnOp(op,ex) =>
        nameAnalysis(ex)
      case BinOp(ex1,op,ex2) =>
        nameAnalysis(ex1)
        nameAnalysis(ex2)
      case IntLit(_) => 
    }
    nameAnalysisBody(function.body)
  }

  /** Verifies that all variables have been initialized before they are used in
   *  every control flow path. Reports uninitialized variables.
   *
   *  @param program	the program to analyse
   */  
  private def initializedVariables(function: Function) {
    type Env = Set[String]
    def initBlock(env: Env, body: Body): Unit = body match {
      case Assign(Ident(name), exp) :: stmts =>
        initExpr(env, exp)
        initBlock(env + name, stmts)
      case Return(exp) :: stmts =>
        initExpr(env, exp)
        if (stmts.nonEmpty)
          Report.error (stmts.head.pos, "Unreachable code")
      case Nil =>
        Report.error (function.id.pos, "Missing return statement")
    }
    def initExpr(env: Env, exp: Expression): Unit = exp match {
      case IntLit(_) =>
      case id @ Ident(name) =>
        if (!env.contains(name))
          Report.error (id.pos, "Uninitialized variable")
      case UnOp(_, exp) =>
        initExpr(env, exp)
      case BinOp(left, _, right) =>
        initExpr(env, left)
        initExpr(env, right)
    }
    initBlock(Set.empty[String], function.body)
  }
}
