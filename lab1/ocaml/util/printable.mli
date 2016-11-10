(* C0 Compiler
 * Printable maps and sets
 * Author: Michael Duggan <md5i@cs.cmu.edu>
 *)

module type POrderedType =
  sig
    type t
    val compare : t -> t -> int
    val format : Format.formatter -> t -> unit
  end

module type SET =
  sig
    include Set.S
    val format : Format.formatter -> t -> unit
  end

module type MAP =
  sig
    include Map.S
    val find' : key -> 'a t -> 'a option (* find without exceptions*)
    val format : (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit
  end

module MakeSet (P : POrderedType) : SET with type elt = P.t
module MakeMap (P : POrderedType) : MAP with type key = P.t

