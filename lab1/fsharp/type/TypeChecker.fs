/// <summary>
/// Lab 1 Compiler: Type Checker
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file replaces typechecker.sml from the SML starter code.
///
/// Simple typechecker that is based on a unit Symbol.table
/// This is all that is needed since there is only an integer type present.
/// Also, since only straightline code is accepted, we hack our way
/// around initialization checks here.
/// </remarks>

#light

namespace Lab1.Type

open System
open Lab1.Parse
open Lab1.Util

module TypeChecker =
    module A = Ast
    module S = Symbol

    let rec private checkExp env ast ext =
        match ast with
        | A.Var id ->
            match S.look env id with
            | None ->
                let errorMsg =
                    sprintf "undeclared variable `%s'" (S.name id)
                ErrorMsg.error ext errorMsg
                raise ErrorMsg.Error
            | Some false ->
                let errorMsg =
                    sprintf "uninitialized variable `%s'" (S.name id)
                ErrorMsg.error ext errorMsg
                raise ErrorMsg.Error
            | Some true -> ()
        | A.ConstExp c -> ()
        | A.OpExp(oper, es) ->
            // Note: it is syntactically impossible in this language to
            // apply an operator to an incorrect number of arguments
            // so we only check each of the arguments
            for e in es do
                checkExp env e ext
        | A.Marked markedExp ->
            checkExp env (Mark.data markedExp) (Mark.ext markedExp)

    let rec private checkStms env stms ext ret =
        match stms with
        | [] -> ret
        | A.Decl(id)::stms' ->
            match Symbol.look env id with
            | Some _ ->
                let errorMsg =
                    sprintf "redeclared variable '%s'" (S.name id)
                ErrorMsg.error None errorMsg
                raise ErrorMsg.Error
            | None ->
                checkStms (S.bind env (id, false)) stms' ext ret
        | A.Assign(id, e)::stms' ->
            checkExp env e ext
            let errorMsg = sprintf "undeclared variable `%s'" (S.name id)
            match Symbol.look env id with
            | None ->
                ErrorMsg.error ext errorMsg
                raise ErrorMsg.Error
            // just got initialized
            | Some false -> checkStms (S.bind env (id, true)) stms' ext ret
            // already initialized
            | Some true -> checkStms env stms' ext ret
        | (A.Return e)::stms' ->
            checkExp env e ext
            checkStms env stms' ext true
        | (A.Markeds marked_stm)::stms' ->
            let stms'' = (Mark.data marked_stm)::stms'
            checkStms env stms'' (Mark.ext marked_stm) ret

    // prints error message and raises ErrorMsg.error if error found
    let typecheck prog =
        if checkStms S.empty prog None false then
            ()
        else
            ErrorMsg.error None "main does not return\n"
            raise ErrorMsg.Error

