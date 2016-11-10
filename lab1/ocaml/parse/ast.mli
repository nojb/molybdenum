(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Forward compatible fragment of C0
 *)

type ident = Symbol.symbol

type oper =
   PLUS
 | MINUS
 | TIMES
 | DIVIDEDBY
 | MODULO
 | NEGATIVE                     (* unary minus *)

type exp =
   Var of ident
 | ConstExp of Int32.t
 | OpExp of oper * exp list
 | Marked of exp Mark.marked
and stm =
   Assign of ident * exp
 | Return of exp
 | Markeds of stm Mark.marked

type decl = ident

type program = (decl list * stm list)

(* print as source, with redundant parentheses *)
module type PRINT =
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end

module Print : PRINT
