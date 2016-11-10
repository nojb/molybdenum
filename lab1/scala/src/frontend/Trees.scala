package frontend

import scala.language.implicitConversions
import scala.util.parsing.input.Positional

/**
 *  The abstract syntax tree for L1
 * @author Robin Steiger
 * @revision Andre Platzer
 */
object Trees {
  
  // The root of the AST
  sealed trait Tree extends Positional {
    override def toString = PrettyPrinter(this)
  }
  
  // Common subtypes
  sealed trait Expression extends Tree
  sealed trait Statement extends Tree
  type Body = List[Statement]
  
  // The nodes in the AST
  case class Program(mainFunction: Function) extends Tree
  case class Function(id: Ident, decls: List[Ident], body: Body) extends Tree
  case class Assign(id: Ident, expr: Expression) extends Statement
  case class Return(expr: Expression) extends Statement
  case class IntLit(value: Int) extends Expression
  case class Ident(name: String) extends Expression
  case class UnOp(op: Primitive, exp: Expression) extends Expression
  case class BinOp(left: Expression, op: Primitive, right: Expression) extends Expression

  // Data type for primitive operations
  sealed abstract class Primitive(val name: String) {
    override def toString: String = name
  }
  case object Add extends Primitive("+")
  case object Sub extends Primitive("-")
  case object Mul extends Primitive("*")
  case object Div extends Primitive("/")
  case object Mod extends Primitive("%")
  case object Negate extends Primitive("-")
    
  // Pretty Printer for ASTs
  import scala.text.{Document, DocBreak}
  import Document._
    
  object PrettyPrinter {
    def apply(tree: Tree) = writeString(pp(tree))
    
    private val indent = 2
    private val docWidth = 80
    
    private def writeString(doc: Document) = {
      val writer = new java.io.StringWriter()
      doc.format(docWidth, writer)
      writer.flush()
      writer.toString
    }

     // Helper methods
    private val line: Document = DocBreak

    private def paren(d: Document): Document =
      group("(" :: nest(indent, d) :: ")")

    private def block(d: Document): Document =
      group("{" :: nest(indent, d) :/: "}")

    private def repsep(doc: Seq[Document], sep: Document): Document =
      if (doc.isEmpty) empty else
        doc.reduceLeft {(rest, d) => rest :: sep :: d}

    private def ppDecl(id: Ident): Document =
      group("int" :/: pp(id) :: ";")
    
    // Pretty print a tree
    private def pp(tree: Tree): Document = tree match {
      case Program(main) =>
        pp(main)
      case Function(id, decls, stmts) =>
        val body = (decls map ppDecl) ::: (stmts map pp)
        group("int" :/: pp(id) :: "()") :/: block(
          (body foldLeft (empty: Document)){_ :/: _}
        )
      case Assign(id, ex) =>
        group(nest(indent, pp(id) :: " =" :/: pp(ex) :: ";"))
      case Return(ex) =>
        group(nest(indent, "return" :/: pp(ex) :: ";"))
      case IntLit(v) =>
        v.toString
      case Ident(n) =>
        n
      case UnOp(op, ex) =>
        paren(op :: pp(ex))
      case BinOp(l, op, r) =>
        paren(pp(l) :: (" " + op) :/: pp(r))
    }
    private implicit def stringToDoc(s: String): Document = text(s)
    private implicit def primitiveToDoc(op: Primitive): Document = text(op.toString)
  }
}
