(* L1 Compiler
 * Simple structure for cleanly handling input parameters
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type flag

val flag : string -> flag (* create a new flag that is not set *)
val not : flag -> flag    (* reverses the meaning of flag being set *)

val set : flag -> unit                  (* set a flag *)
val unset : flag -> unit                (* unset a flag *)
val isset : flag -> bool                (* check if the flag is set *)

(* return a function that runs only if flag is set *)
val guard : flag -> ('a -> unit) -> 'a -> unit


(* return a function that runs only if all flags are set *)
val guards : flag list -> ('a -> unit) -> 'a -> unit

(* return a func that runs if one of flags is set *)
val someguards : flag list -> ('a -> unit) -> 'a -> unit

(* get a function that runs the first one if flag is set, or second
   one if it is not *)
val branch : flag -> ('a -> 'b) * ('a -> 'b) -> 'a -> 'b

val show : flag -> string (* returns string that contains the setting
                             of the flag *)
