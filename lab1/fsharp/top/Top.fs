/// <summary>
/// Lab 1 Compiler: Top level Environment
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file replaces top.sml from the SML starter code.
/// </remarks>

#light

namespace Lab1.Top

open System
open System.IO

open Lab1.CodeGen
open Lab1.Parse
open Lab1.Trans
open Lab1.Type

open Lab1.Util.Flags

module TopLevel =

    let parseAndTypecheck (flags:CommandLineFlags) source =
        if flags.Verbose then
            printfn "Parsing %s" source
        let ast = FileParser.parse source
        if flags.DumpAst then
            printfn "%s" (Ast.Print.printProgram ast)

        if flags.Verbose then
            printfn "Checking"
        ignore(TypeChecker.typecheck ast)

        ast
    
    let translate (flags:CommandLineFlags) ast =
        if flags.Verbose then
            printfn "Translating"
        let ir = Trans.translate ast
        if flags.DumpIR then
            printfn "%s" (Tree.Print.printProgram ir)

        ir

    let selectInstructions (flags:CommandLineFlags) ir =
        if flags.Verbose then
            printfn "Codegen"
        Codegen.codegen ir

    let compile (flags:CommandLineFlags) source =
        ignore(Temp.reset())

        let ast = parseAndTypecheck flags source
        let ir = translate flags ast
        let assem =
            [InterRep.DIRECTIVE(sprintf ".file\t\"%s\"" source)]
            @ (selectInstructions flags ir)
            @ [InterRep.DIRECTIVE ".ident\t\"L1 Starter Code\""]

        // TODO: The translation into true x64 assembly goes here
        let code = String.Join("\n", List.map InterRep.format assem) + "\n"

        let outfile = Path.ChangeExtension(source, ".s")
        if flags.Verbose then
            printfn "Writing assembly to %s" outfile

        File.WriteAllText(outfile, code)

    [<EntryPoint>]
    let main args =
        let flags = new CommandLineFlags(args)
        if flags.Help then
            flags.PrintHelp()
            0
        else
            match flags.UnprocessedArgs with
            | [] ->
                eprintfn "ERROR: No input files"
                flags.PrintHelp()
                1
            | [source] ->
                try
                    compile flags source
                    0
                with
                | ex ->
                    if flags.PrintStackTrace then
                        printfn "%s\n" ex.StackTrace
                    else
                        printfn "%s\n" ex.Message
                    2
            | _ ->
                eprintfn "ERROR: More than one input file"
                flags.PrintHelp()
                1

