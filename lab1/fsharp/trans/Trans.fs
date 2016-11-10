(* L1 Compiler
 * AST -> IR Translator
 * Ported from SML source by bhamme
 *)

namespace Lab1.Trans

open Lab1.Parse
open Lab1.Util

module Trans = 

    let transOper o =
        match o with
        | Ast.Plus -> Tree.ADD
        | Ast.Minus -> Tree.SUB
        | Ast.Times -> Tree.MUL
        | Ast.DividedBy -> Tree.DIV
        | Ast.Modulo -> Tree.MOD
        | Ast.Negative -> Tree.SUB (* unary to binary! *)

    let rec transExp env exp = 
        match exp with
        | Ast.Var id -> Tree.TEMP(Symbol.look' env id)
        (* after type-checking, id must be declared; do not guard lookup *)
        | Ast.ConstExp c -> Tree.CONST c
        | Ast.OpExp(op, [e1; e2]) ->
            Tree.BINOP(transOper op, transExp env e1, transExp env e2)
        | Ast.OpExp(Ast.Negative , [e]) ->
            Tree.BINOP(transOper Ast.Negative,
                Tree.CONST 0,
                transExp env e)
        | Ast.OpExp(_, _) -> failwith "IMPOSSIBLE"
        | Ast.Marked markedExp -> transExp env (Mark.data markedExp)

    (* translate the statement *)
    (* trans_stms : Temp.temp Symbol.table -> A.stm list -> Tree.stm list *)
    let rec transStms env s =
        match s with
        | (Ast.Decl(id)::stms) -> transStms env stms
        | (Ast.Assign(id, e))::stms -> 
            let t = Temp.create()
            let env' = Symbol.bind env (id, t)
            Tree.MOVE(Tree.TEMP t, transExp env e)::(transStms env' stms)
        | (Ast.Return e)::_ -> Tree.RETURN(transExp env e)::[]
        (* ignore code after return *)
        | (Ast.Markeds markedStm)::stms ->
            transStms env ((Mark.data markedStm)::stms)
        | [] -> failwith "Missing return statement"

    let translate stms = transStms Symbol.empty stms

