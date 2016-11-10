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

(* Consider using pretty printing *)

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

module type PRINT =
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end

module Print : PRINT =
  struct

    let pp_ident id = Symbol.name id

    let pp_oper = function
        PLUS -> "+"
      | MINUS -> "-"
      | TIMES -> "*"
      | DIVIDEDBY -> "/"
      | MODULO -> "%"
      | NEGATIVE -> "-"

    let rec pp_exp = function
        Var id     -> pp_ident id
      | ConstExp c -> Int32.to_string c
      | OpExp (op, [e]) -> pp_oper op ^ "(" ^ pp_exp e ^ ")"
      | OpExp (op, [e1; e2]) ->
          "(" ^ pp_exp e1 ^ " " ^ pp_oper op ^ " " ^ pp_exp e2 ^ ")"
      | Marked exp -> pp_exp (Mark.data exp)
      | _ -> assert false

    let rec pp_stm = function
        Assign (id, e) -> pp_ident id ^ " = " ^ pp_exp e ^ ";"
      | Return e -> "return " ^ pp_exp e ^ ";"
      | Markeds stm -> pp_stm (Mark.data stm)

    let rec pp_stms = function
        [] -> ""
      | s::ss -> pp_stm s ^ "\n" ^ pp_stms ss

    let pp_decl d = "int " ^ pp_ident d ^ ";"

    let rec pp_decls = function
        [] -> ""
      | d::dd -> pp_decl d ^ "\n" ^ pp_decls dd

    let pp_program (decls, stms) = "{\n" ^ pp_decls decls ^ pp_stms stms ^ "}"
  end
