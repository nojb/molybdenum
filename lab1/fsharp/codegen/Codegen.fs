(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Ported from SML by bhamme
 * 
 * Implements a "convenient munch" algorithm
 *)

namespace Lab1.CodeGen

open Lab1.Trans

module Codegen = 
    module T = Tree
    module IR = InterRep

    let munchOp op =
        match op with
        | T.ADD -> IR.ADD
        | T.SUB -> IR.SUB
        | T.MUL -> IR.MUL
        | T.DIV -> IR.DIV
        | T.MOD -> IR.MOD

    (* munchExp : IR.operand -> T.exp -> IR.instr list *)
    (* munchExp d e
     * generates instructions to achieve d <- e
     * d must be TEMP(t) or REG(r)
     *)
    let rec munchExp d e =
        match e with
        | (T.CONST(n)) -> [IR.MOV(d, IR.IMM(n))]
        | (T.TEMP(t)) -> [IR.MOV(d, IR.TEMP(t))]
        | (T.BINOP(binop, e1, e2)) -> munchBinop d (binop, e1, e2)

    (* munchBinop : IR.operand -> T.binop * T.exp * T.exp -> IR.instr list *)
    (* munchBinop d (binop, e1, e2)
     * generates instruction to achieve d <- e1 binop e2
     * d must be TEMP(t) or REG(r)
     *)
    and munchBinop d (binop, e1, e2) =
        let operator = munchOp binop
        let t1 = IR.TEMP(Temp.create())
        let t2 = IR.TEMP(Temp.create())
        (munchExp t1 e1) @ (munchExp t2 e2)
            @ [IR.BINOP(operator, d, t1, t2)]

    (* munchStm : T.stm -> IR.instr list *)
    (* munchStm stm generates code to execute stm *)
    let munchStm stm =
        match stm with
        | (T.MOVE(T.TEMP(t1), e2)) -> munchExp (IR.TEMP(t1)) e2
        | (T.RETURN(e)) -> munchExp (IR.REG(IR.EAX)) e
            // return e is implemented as %eax <- e
        | _ ->
            sprintf "Unmatched argument in Codegen.munchStm\n%A" stm
            |> failwith

    let codegen = List.collect munchStm

