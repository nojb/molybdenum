(* L1 Compiler
 * Error messages
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

let anyErrors = ref false

let reset () = anyErrors := false

let msg str ext note =
  anyErrors := true;
  (match ext with
    None -> ()
  | Some x -> print_string (Mark.show x));
  List.iter print_string [":"; str; ":"; note; "\n"]

let error ext note =
  anyErrors := true;
  msg "error" ext note

let warn ext note = msg "warning" ext note

exception Error
