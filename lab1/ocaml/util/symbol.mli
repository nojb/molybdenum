(* C0 Compiler
 * The symbol tables
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type symbol
type t = symbol

val compare : symbol -> symbol -> int (* compare symbols by their
                                         creation time *)



val bogus : symbol              (* a dummy symbol, less than others *)
val is_bogus : symbol -> bool

val reset : unit -> unit (* resets the hash table in which the
                            symbols are stored *)

val symbol : string -> symbol (* generates a new symbol with given name *)
val name : symbol -> string     (* returns a name associated with symbol *)
val format : Format.formatter -> symbol -> unit

(* Sets of symbols *)
module type SET = Printable.SET with type elt = t
module Set : SET

(* Maps of symbols *)
module type MAP = Printable.MAP with type key = t
module Map : MAP

