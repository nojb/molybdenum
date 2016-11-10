(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type temp = int
type t = temp

let counter = ref 1

let reset () = counter := 1
let create () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t

let name t = "%t" ^ string_of_int t
let compare a b = compare a b

let format ff t = Format.fprintf ff "%s" (name t)

module S =
  struct
    type t = temp
    let compare = compare
    let format = format
  end

module type SET = Printable.SET with type elt = t
module Set = Printable.MakeSet(S)

module type MAP = Printable.MAP with type key = t
module Map = Printable.MakeMap(S)
