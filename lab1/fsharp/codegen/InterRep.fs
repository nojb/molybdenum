/// <summary>
/// Lab 1 Compiler: Assembly Language
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file replaces assem.sml from the SML starter code.
///
/// Currently just a pseudo language with 3-operand
/// instructions and arbitrarily many temps.
///
/// We write
///
/// BINOP  operand1 <- operand2,operand3
/// MOV    operand1 <- operand2
/// </remarks>

#light

namespace Lab1.CodeGen

open System
open Lab1.Trans

module InterRep =
    type reg =
        | EAX

    type operand =
        | IMM of int
        | REG of reg
        | TEMP of Temp.temp

    type operation = | ADD | SUB | MUL | DIV | MOD

    type instr =
        | BINOP of operation * operand * operand * operand
        | MOV of operand * operand
        | DIRECTIVE of string
        | COMMENT of string

    // functions that format assembly output

    let formatReg register =
        match register with
        | EAX -> "%eax"

    let formatBinop binop =
        match binop with
        | ADD -> "ADD"
        | SUB -> "SUB"
        | MUL -> "MUL"
        | DIV -> "DIV"
        | MOD -> "MOD"

    let formatOperand oper =
        match oper with
        | IMM n -> sprintf "$%d" n
        | TEMP t -> Temp.name t
        | REG r -> formatReg r

    let format instruction =
        match instruction with
        | BINOP(oper, d, s1, s2) ->
            sprintf "\t%s\t%s <- %s,%s"
                    (formatBinop oper)
                    (formatOperand d)
                    (formatOperand s1)
                    (formatOperand s2)
        | MOV(d, s) ->
            sprintf "\tMOV\t%s <- %s"
                    (formatOperand d)
                    (formatOperand s)
        | DIRECTIVE str ->
            sprintf "\t%s" str
        | COMMENT str ->
            sprintf "\t/* %s */" str

