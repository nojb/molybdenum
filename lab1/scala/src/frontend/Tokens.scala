package frontend

import scala.util.parsing.combinator.token.Tokens

/**
 * Tokens for the L1 language, used by the lexical analyzer (a.k.a.
 * the scanner).
 * @author Robin Steiger
 * @revision Andre Platzer
 * @revision Sri Raghavan
 */
trait L1Tokens extends Tokens {
  abstract class L1Token extends Token {
    override def toString: String = chars
  }
  
  /* keywords */
  case object IntType extends L1Token { def chars = "int" }
  case object Return extends L1Token { def chars = "return" }
  
  /* values */
  case class Ident(val chars: String) extends L1Token
  case class IntLit(n: Int) extends L1Token { def chars = n.toString }
//   case class StringLit(s: String) extends L1Token { def chars = '"'+ s +'"' }
//   case class CharLit(c: Char) extends L1Token { def chars = "'"+ c +"'" }
    
  /* parenthesis */
  case object LParen extends L1Token { def chars = "(" }
  case object RParen extends L1Token { def chars = ")" }
  case object LBrace extends L1Token { def chars = "{" }
  case object RBrace extends L1Token { def chars = "}" }
  case object Semicolon extends L1Token { def chars = ";" }
  
  /* assignments */
  case object Eql extends L1Token { def chars = "=" }
  case object PlusEql extends L1Token { def chars = "+=" }
  case object MinusEql extends L1Token { def chars = "-=" }
  case object StarEql extends L1Token { def chars = "*=" }
  case object SlashEql extends L1Token { def chars = "/=" }
  case object PercentEql extends L1Token { def chars = "%=" }
    
  /* operators */
  case object Plus extends L1Token { def chars = "+" }
  case object Minus extends L1Token { def chars = "-" }
  case object Star extends L1Token { def chars = "*" }
  case object Slash extends L1Token { def chars = "/" }
  case object Percent extends L1Token { def chars = "%" }
}
