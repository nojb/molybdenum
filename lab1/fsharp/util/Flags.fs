/// <summary>
/// Lab 1 Compiler: Class for handling command-line flags
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file is intended to replace flag.sml from the SML starter code.
/// However, this class, unlike the struct defined in flag.sml, is
/// specific to the command-line arguments that this program accepts.
///
/// This file relies on the Mono.Options library, which consists of
/// a single file that can be found locally at external/Options.cs or
/// externally at http://goo.gl/O5EjH (redirects to GitHub).
/// The Mono.Options library is under the MIT license.
/// </remarks>

#light

namespace Lab1.Util

open System

module Flags =
    type CommandLineFlags(args:string[]) =
        let help = ref false
        let verbose = ref false
        let printStackTrace = ref false
        let dumpAst = ref false
        let dumpIR = ref false
        let unprocessedArgs = ref []

        let optionSet = new Mono.Options.OptionSet()
        do
            ignore(optionSet.Add("h|help",
                    "help",
                    fun _ -> help := true))
            ignore(optionSet.Add("v|verbose",
                    "verbose messages",
                    fun _ -> verbose := true))
            ignore(optionSet.Add("s|stack-trace",
                    "print stack trace on exceptions",
                    fun _ -> printStackTrace := true))
            ignore(optionSet.Add("dump-ast",
                    "pretty print the AST",
                    fun _ -> dumpAst := true))
            ignore(optionSet.Add("dump-ir",
                    "pretty print the IR",
                    fun _ -> dumpIR := true))
            unprocessedArgs := List.ofSeq(optionSet.Parse args)

        member this.Help = !help
        member this.Verbose = !verbose
        member this.PrintStackTrace = !printStackTrace
        member this.DumpAst = !dumpAst
        member this.DumpIR = !dumpIR
        member this.UnprocessedArgs = !unprocessedArgs
        member this.PrintHelp() =
            eprintfn "Usage: compile [OPTION...] SOURCEFILE"
            eprintfn "where OPTION is"
            optionSet.WriteOptionDescriptions Console.Error

