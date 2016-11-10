(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Simple typechecker that is based on a unit Symbol.table
 * This is all that is needed since there is only an integer type present
 * Also, since only straightline code is accepted, we hack our way
 * around initialization checks here.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 *)

module A = Ast
module S = Symbol.Map

(* tc_exp : unit Symbol.Map.t -> Ast.exp -> Mark.ext option -> unit *)
let rec tc_exp env ast ext =
  match ast with
    A.Var id ->
      (match S.find' id env with
      | None -> ErrorMsg.error ext
          ("undeclared variable `" ^ Symbol.name id ^ "'");
        raise ErrorMsg.Error
      | Some false -> ErrorMsg.error ext
          ("uninitialized variable `" ^ Symbol.name id ^ "'") ;
          raise ErrorMsg.Error
      | Some true -> ())
  | A.ConstExp c -> ()
  | A.OpExp (oper,es) ->
      (* Note: it is syntactically impossible in this language to
       * apply an operator to an incorrect number of arguments
       * so we only check each of the arguments
       *)
      List.iter (fun e -> tc_exp env e ext) es
  | A.Marked marked_exp ->
      tc_exp env (Mark.data marked_exp) (Mark.ext marked_exp)

(* tc_stms :
     unit Symbol.Map.t -> Ast.program -> Mark.ext option -> bool -> unit *)
let rec tc_stms env ast ext ret =
  match ast with
    [] -> ret
  | A.Assign(id,e)::stms ->
      tc_exp env e ext;
      (match S.find' id env with
        None -> ErrorMsg.error ext
            ("undeclared variable `" ^ Symbol.name id ^ "'");
          raise ErrorMsg.Error
            (* just got initialized *)
      | Some false -> tc_stms (S.add id true env) stms ext ret
            (* already initialized *)
      | Some true -> tc_stms env stms ext ret)
  | A.Return(e)::stms ->
      tc_exp env e ext;
      tc_stms env stms ext true
  | A.Markeds(marked_stm)::stms ->
      tc_stms env ((Mark.data marked_stm)::stms) (Mark.ext marked_stm) ret

(* populate environment with declarations. false is for uninitialized. *)
let rec typecheck'  = function
    ([], stms) -> fun env -> tc_stms env stms None false
  | (decl::decls, stms) -> fun env ->
      match S.find' decl env with
        Some _ -> ErrorMsg.error None
            ("redeclared variable `" ^ Symbol.name decl ^ "'") ;
          raise ErrorMsg.Error
      | None -> typecheck' (decls, stms) (S.add decl false env)

let typecheck prog =
  if typecheck' prog S.empty then ()
  else (ErrorMsg.error None "main does not return\n"; raise ErrorMsg.Error)
