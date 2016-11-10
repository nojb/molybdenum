/// <summary>
/// Lab 1 Compiler: Abstract Syntax Trees
///
/// Lexes forward compatible fragment of C0.
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file is intended to replace ast.sml from the SML starter code.
/// </remarks>

#light

namespace Lab1.Parse

open Lab1.Util

module Ast =
    type ident = Symbol.symbol

    type oper = | Plus | Minus | Times | DividedBy | Modulo | Negative

    type exp =
        | Var of ident
        | ConstExp of int
        | OpExp of oper * exp list
        | Marked of exp Mark.marked

    type stm =
        | Decl of ident
        | Assign of ident * exp
        | Return of exp
        | Markeds of stm Mark.marked

    type program = stm list

    // print programs and expressions in source form using redundant
    // parentheses to clarify precedence
    module Print =
        let printIdent = Symbol.name

        let printOper oper =
            match oper with
            | Plus -> "+"
            | Minus -> "-"
            | Times -> "*"
            | DividedBy -> "/"
            | Modulo -> "%"
            | Negative -> "-"

        let rec printExp exp =
            match exp with
            | Var id -> printIdent id
            | ConstExp c -> sprintf "%d" c
            | OpExp(oper, [e]) ->
                sprintf "%s(%s)" (printOper oper) (printExp e)
            | OpExp(oper, [e1; e2]) ->
                sprintf "(%s %s %s)" (printExp e1) (printOper oper)
                    (printExp e2)
            | OpExp(_, _) -> failwith "IMPOSSIBLE"
            | Marked marked_exp -> printExp(Mark.data marked_exp)

        let rec printStm stm =
            match stm with
            | Decl id -> sprintf "int %s" (printIdent id)
            | Assign(id, e) ->
                sprintf "%s = %s;" (printIdent id) (printExp e)
            | Return e -> sprintf "return %s;" (printExp e)
            | Markeds marked_stm -> printStm(Mark.data marked_stm)

        let rec printStms stms =
            match stms with
            | [] -> ""
            | s::ss -> sprintf "%s\n%s" (printStm s) (printStms ss)

        let printProgram stms =
            sprintf "{\n%s}" (printStms stms)

