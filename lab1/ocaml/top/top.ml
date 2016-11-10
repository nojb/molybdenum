(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

let say = prerr_endline
let newline = prerr_newline

let flag_verbose = Flag.flag "verbose"
let flag_ast = Flag.flag "ast"
let flag_ir = Flag.flag "ir"
let flag_assem = Flag.flag "assem"
let flag_parse = Flag.flag "parse"

let reset_flags () =
  List.iter Flag.unset [flag_verbose; flag_ast; flag_ir; flag_assem]

let set flag = Arg.Unit (fun () -> Flag.set flag)

let options =
  [("--verbose", set flag_verbose, "verbose message");
   ("-v", set flag_verbose, "verbose message");
   ("--debug-parse", set flag_parse, "debug the parser");
   ("--dump-ast", set flag_ast, "pretty print the AST");
   ("--dump-ir", set flag_ir, "pretty print the IR");
   ("--dump-assem", set flag_assem,
    "pretty print the assembly before register allocation")
 ]

exception EXIT

let stem s =
  try
    let dot = String.rindex s '.' in
    String.sub s 0 dot
  with Not_found -> s

let (@@) f g x = f (g x)

let main args =
  try
    let header = "Usage: compile [OPTION...] SOURCEFILE\nwhere OPTION is" in
    let usageinfo () = Arg.usage options header in
    let errfn msg = say (msg ^ "\n"); usageinfo (); raise EXIT in

    let _ = Temp.reset () in
    let _ = reset_flags () in

    let _ = if Array.length args < 2 then (usageinfo (); raise EXIT) in

    (* Parse the arguments.  Non-keyword arguments accumulate in fref,
       and then is assigned to files. *)
    let files =
      let fref = ref []
      and current = ref 0 in
      let () = Arg.parse_argv ~current args options
          (fun s -> fref := s::!fref) header in
      List.rev !fref in

    let source = match files with
      [] -> errfn "Error: no input file"
    | [filename] -> filename
    | _ -> errfn "Error: more than one input file" in

    (* Parse *)
    let _ = Flag.guard flag_verbose say ("Parsing... " ^ source) in
    let _ = Flag.guard flag_parse
        (fun _ -> ignore (Parsing.set_trace true)) () in
    let ast = Parse.parse source in
    let _ = Flag.guard flag_ast
        (fun () -> say (Ast.Print.pp_program ast)) () in

    (* Typecheck *)
    let _ = Flag.guard flag_verbose say "Checking..." in
    let _ = TypeChecker.typecheck ast in

    (* Translate *)
    let _ = Flag.guard flag_verbose say "Translating..." in
    let ir = Trans.translate ast in
    let _ = Flag.guard flag_ir (fun () -> say (Tree.Print.pp_program ir)) () in

    (* Codegen *)
    let _ = Flag.guard flag_verbose say "Codegen..." in
    let assem = Codegen.codegen ir in
    let _ = Flag.guard flag_assem
        (fun () -> List.iter (print_string @@ Assem.format) assem) () in

    (* Add assembly header and footer *)
    let assem =
      [Assem.DIRECTIVE(".file\t\"" ^ source ^ "\"")]
      @ assem
      @ [Assem.DIRECTIVE ".ident\t\"15-411 L1 reference compiler\""] in
    let code = String.concat "" (List.map Assem.format assem) in

    (* Output assembly *)
    let afname = stem source ^ ".s" in
    let _ = Flag.guard flag_verbose
        say ("Writing assembly to " ^ afname ^ " ...") in
    let _ = SafeIO.withOpenOut afname
        (fun afstream -> output_string afstream code) in
    (* Return success status *)
    0

  with
    ErrorMsg.Error -> say "Compilation failed"; 1
  | EXIT -> 1
  | Arg.Help x -> prerr_string x; 1
  | e -> say "Unrecognized exception"; 1


let test s =
  main (Array.of_list (""::(Str.split (Str.regexp "[ \t\n]+") s)))
