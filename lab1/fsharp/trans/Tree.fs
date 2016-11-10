(* L1 Compiler
 * IR Trees
 * Ported from SML source by bhamme
 *)

namespace Lab1.Trans

open System

module Tree = 
    
    type binop = | ADD | SUB | MUL | DIV | MOD

    type exp =
    | CONST of Int32
    | TEMP of Temp.temp
    | BINOP of binop * exp * exp

    type stm = 
    | MOVE of exp * exp
    | RETURN of exp

    type program = stm list

    module Print = 
        let printBinop b =
            match b with
            | ADD -> "+"
            | SUB -> "-"
            | MUL -> "*"
            | DIV -> "/"
            | MOD -> "%"
        
        let rec printExp e =
            match e with
            | (CONST (x)) -> sprintf "%d" x
            | (TEMP (t)) -> Temp.name t
            | (BINOP (binop, e1 , e2 )) ->
                sprintf "(%s %s %s)" (printExp e1)
                        (printBinop binop) (printExp e2)

        let printStm s =
            match s with
            | (MOVE ( e1 , e2 )) ->
                sprintf "%s  <--  %s" (printExp e1) (printExp e2)
            | (RETURN e) -> sprintf "return %s" (printExp e)

        let rec printProgram p =
            match p with
            | [] -> ""
            | (stm::stms) ->
                sprintf "%s\n%s" (printStm stm) (printProgram stms)

