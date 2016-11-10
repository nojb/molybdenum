package frontend

import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.CharArrayReader.EofCh   
import top.Report

/**
 * A lexical analyzer (scanner) for the L1 lexical grammar.
 * @author Robin Steiger
 * @revision Andre Platzer
 */
object L1Scanner extends Scanners with L1Tokens {

  override def whitespace: Parser[Any] = rep(
      anyCharIn(" \t\n\f\r")
    | '/' ~ '*' ~ comment
    | '#' ~ rep( anyCharNotIn("\n" + EofCh) )
    | '/' ~ '/' ~ rep( anyCharNotIn("\n" + EofCh) )
    | '/' ~ '*' ~ failure("unclosed comment")
    )

  private def comment: Parser[Any] = (
      '/' ~ '*' ~ comment ~ comment ^^ { case _ => ' '  }
    | '*' ~ '/'  ^^ { case _ => ' '  }
    | '/' ~ '*' ~ failure("unclosed comment")
    | '/' ~ anyCharNotIn("*" + EofCh) ~ comment
    | anyCharNotIn("/" + EofCh) ~ comment
    )
      
  override def token: Parser[Token] = (
      identStart ~! rep(identStart | digit)
        ^^ { case l ~ ls => processIdent(l :: ls) }
//     | '"' ~> rep(anyCharNotIn("\n\r\"" + EofCh)) <~ '"'
//         ^^ { chars => StringLit(charsToString(chars)) }
//     | '\'' ~> anyCharNotIn(EofCh.toString) <~ '\''
//         ^^ CharLit
    | rep1(digit)
        ^^ { chars => 
			if (BigInt(charsToString(chars)) >> 32 == 0)
			    IntLit(charsToString(chars).toLong.toInt)
			else {
			    //@todo add position reporting
			    Report error "integer constant is too large: " + chars
  			    IntLit(0)
			}
			}
    | '(' ^^^ LParen
    | ')' ^^^ RParen
    | '{' ^^^ LBrace
    | '}' ^^^ RBrace
    | ';' ^^^ Semicolon
    | '=' ^^^ Eql
    | '+' ~ '=' ^^^ PlusEql
    | '-' ~ '=' ^^^ MinusEql
    | '*' ~ '=' ^^^ StarEql
    | '/' ~ '=' ^^^ SlashEql
    | '%' ~ '=' ^^^ PercentEql
    | '+' ^^^ Plus
    | '-' ^^^ Minus
    | '*' ^^^ Star
    | '/' ^^^ Slash
    | '%' ^^^ Percent
    | EofCh ^^^ EOF
    | failure("illegal character"))

  private def digit: Parser[Char] =
    anyCharIn("0123456789")
  private def identStart: Parser[Char] =
    anyCharIn("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")

  private def anyCharIn(set: String): Parser[Char] =
    elem("any in {" + set + "}",
         { c: Char => (set indexOf c.toLower) != -1 })
  private def anyCharNotIn(set: String): Parser[Char] =
    elem("any not in {" + set + "}",
         { c: Char => (set indexOf c.toLower) == -1 })

  private def processIdent(chars: List[Char]): Token =
    charsToString(chars) match {
      case "int"         => IntType
      case "return"      => Return
      case ident         => Ident(ident)
    }

  private def charsToString(chars: List[Char]): String =
    chars mkString ""
}
  
