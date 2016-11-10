/// <summary>
/// Lab 1 Compiler: Wrapper around Parser
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
/// </remarks>

#light

namespace Lab1.Parse

open System.IO
open Microsoft.FSharp.Text.Lexing

module FileParser =
    let parse sourceFile =
        let contents = File.ReadAllText sourceFile
        let lexbuf = LexBuffer<char>.FromString contents
        ignore(Lex.setInitialPos lexbuf sourceFile)
        Parser.program Lex.token lexbuf

