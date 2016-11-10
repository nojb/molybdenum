(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 * We write
 *
 * BINOP  operand1 <- operand2,operand3
 * MOV    operand1 <- operand2
 *
 *)

type reg = EAX

type operand =
    IMM of Int32.t
  | REG of reg
  | TEMP of Temp.temp

type operation = ADD | SUB| MUL | DIV | MOD

type instr =
    BINOP of operation * operand * operand * operand
  | MOV of operand * operand
  | DIRECTIVE of string
  | COMMENT of string

(* functions that format assembly output *)

let format_reg = function
    EAX -> "%eax"

let format_binop = function
    ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"

let format_operand = function
    IMM n  -> "$" ^ Int32.to_string n
  | TEMP t -> Temp.name t
  | REG r  -> format_reg r

let format = function
  | BINOP (oper, d, s1, s2) ->
      "\t" ^ format_binop oper
      ^ "\t" ^ format_operand d
      ^ " <- " ^ format_operand s1
      ^ "," ^ format_operand s2 ^ "\n"
  | MOV (d, s) ->
      "\t" ^ "MOV"
      ^ "\t" ^ format_operand d
      ^ " <- " ^ format_operand s ^ "\n"
  | DIRECTIVE str ->
      "\t" ^ str ^ "\n"
  | COMMENT str ->
      "\t" ^ "/* " ^ str ^ "*/\n"
