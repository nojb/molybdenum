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

val format : instr -> string
