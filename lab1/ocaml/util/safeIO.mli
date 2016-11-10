(* L1 Compiler
 * Safe(r) I/O functions
 * Author: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

(* withOpenIn fileName (fn in_channel => body) = result
   opens fileName for input to obtain instream and evaluates body.
   The file is closed during normal and abnormal exit of body.
*)
val withOpenIn : string -> (in_channel -> 'a) -> 'a

(* withOpenOut fileName (fn out_channel => body) = result
   opens fileName for output to obtain outstream and evaluates body.
   The file is closed during normal and abnormal exit of body.
*)
val withOpenOut : string -> (out_channel -> 'a) -> 'a
