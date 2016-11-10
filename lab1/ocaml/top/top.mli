(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

(* main function for standalone executable
 *)
val main : string array -> int

(* test "arguments"; is the same as executing a saved
 * heap with arguments on the command line
 *)
val test : string -> int
