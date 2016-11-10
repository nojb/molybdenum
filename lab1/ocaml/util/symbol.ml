(* C0 Compiler
 * The symbol tables
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type symbol = string * int
type t = symbol

let bogus = ("?", -1)
let is_bogus = fun (_, x) -> x = -1

let compare (_, i) (_, i') =
  if i < 0 || i' < 0 then 1
  else i - i'

let ht = Hashtbl.create 128
let nexts = ref 0
let reset () = nexts := 0; Hashtbl.clear ht
let symbol name =
  try
    (name, Hashtbl.find ht name)
  with Not_found ->
    let i = !nexts in
    let () = incr nexts in
    let () = Hashtbl.add ht name i in
    (name, i)

let name (n, _) = n

let format ff s =
  Format.fprintf ff "%s" (name s)

module S =
  struct
    type t = symbol
    let compare = compare
    let format = format
  end

module type SET = Printable.SET with type elt = t
module Set = Printable.MakeSet(S)

module type MAP = Printable.MAP with type key = t
module Map = Printable.MakeMap(S)

