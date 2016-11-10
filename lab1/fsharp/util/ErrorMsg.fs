/// <summary>
/// Lab 1 Compiler: Error messages
/// </summary>
/// <remarks>
/// Author: Ben Hamme (bhamme)
/// Author: Adam Mihalcin (amihalci)
/// 15-411 Fall 2012
///
/// This file is intended to replace errormsg.sml from the SML starter code.
/// </remarks>

#light

namespace Lab1.Util

module ErrorMsg =
    let mutable anyErrors = false

    let reset() =
        anyErrors <- false

    let msg str (ext : Mark.ext option) note =
        anyErrors <- true
        match ext with
        | Some ext' -> printf "%s" (Mark.show ext')
        | None -> ()
        printfn ":%s:%s" str note

    let error ext note =
        msg "error" ext note
    
    let warn ext note =
        msg "warning" ext note

    exception Error

