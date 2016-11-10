package frontend

import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical.TokenParsers
import frontend.{Trees => T}

/** A syntactical analyzer (parser) for the L1 syntactical grammar.
 *  Currently this is just a parser 'by combination', which is less
 *  efficient than LL(1) parsing.
 *
 * @author Robin Steiger
 * @revision Andre Platzer
 */

object L1Parser extends TokenParsers {
  type Tokens = L1Tokens
  val lexical = L1Scanner

  import lexical._

  private implicit def accept0(e: Token): Parser[Token] = accept(e)

  def program: Parser[T.Program] =
    phrase(mainProgram)

  private def mainProgram: Parser[T.Program] = positioned(
    function ^^ T.Program
  )

  private def function: Parser[T.Function] = positioned(
    (IntType ~> ident) ~ (LParen ~> RParen ~> LBrace ~> rep(declaration)) ~ (rep(statement) <~ RBrace)
    ^^ { case i ~ d ~ s => T.Function(i, d, s) })

  private def declaration: Parser[T.Ident] = (
    IntType ~> ident <~ Semicolon)

  private def statement: Parser[T.Statement] = (
      simpleStatement <~ Semicolon
    | returnStmt <~ Semicolon)

  private def returnStmt: Parser[T.Return] = positioned(
    Return ~> term ^^ T.Return)

  private def simpleStatement: Parser[T.Statement] = (
    (ident ~ asop) ~ term ^^ {
      case i ~ None ~ t => T.Assign(i, t)
      case i ~ Some(op) ~ t => T.Assign(i, T.BinOp(i, op, t))
    })

  private def expression: Parser[T.Expression] = positioned(
      intconst
    | ident
    | Minus ~> expression ^^ { case e => T.UnOp(T.Negate, e) }
    | LParen ~> term <~ RParen
    )
    
  private def factor: Parser[T.Expression] = positioned(
    expression ~ rep(mulOp ~ expression)
      ^^ { case f ~ ts => (ts foldLeft f)(combineExprs) })
    
  private def term: Parser[T.Expression] = positioned(
    factor ~ rep(addOp ~ factor)
      ^^ { case f ~ ts => (ts foldLeft f)(combineExprs) })

  private def combineExprs(ex1: T.Expression, more: ~[T.Primitive, T.Expression]) = {
    val op ~ ex2 = more
    T.BinOp(ex1, op, ex2)
  }

  private def ident: Parser[T.Ident] = positioned(
    elem("identifier", _.isInstanceOf[Ident])
      ^^ { case Ident(n) => T.Ident(n) })
  
  private def intconst: Parser[T.IntLit] = positioned(
    elem("integer", _.isInstanceOf[IntLit])
      ^^ { case IntLit(n) => T.IntLit(n) })

  private def mulOp: Parser[T.Primitive] = (
      Star ^^^ T.Mul
    | Slash ^^^ T.Div
    | Percent ^^^ T.Mod)

  private def addOp: Parser[T.Primitive] = (
      Plus ^^^ T.Add
    | Minus ^^^ T.Sub)

  private def asop: Parser[Option[T.Primitive]] = (
      Eql ^^^ None
    | PlusEql ^^^ Some(T.Add)
    | MinusEql ^^^ Some(T.Sub)
    | StarEql ^^^ Some(T.Mul)
    | SlashEql ^^^ Some(T.Div)
    | PercentEql ^^^ Some(T.Mod))

}
