(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type temp
type t = temp

val reset : unit -> unit              (* resets temp numbering *)
val create : unit -> temp             (* returns a unique new temp *)
val name : temp -> string             (* returns the name of a temp *)
val compare : temp -> temp -> int     (* comparison function *)

val format : Format.formatter -> temp -> unit


(* Sets of temps *)
module type SET = Printable.SET with type elt = t
module Set : SET

(* Maps of temps *)
module type MAP = Printable.MAP with type key = t
module Map : MAP
