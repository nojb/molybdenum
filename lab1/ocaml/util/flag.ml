(* L1 Compiler
 * Simple structure for cleanly handling input parameters
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type flag = {name : string; mutable value : bool; post : bool -> bool }

let flag name = {name = name; value = false; post = fun b -> b }

let set f = f.value <- true
let unset f = f.value <- false
let not f = {f with post = fun b -> not (f.post b)}

let isset f = f.post f.value

let guard fl f x = if isset fl then f x else ()
let guards fls f x = if List.for_all isset fls then f x else ()
let someguards fls f x = if List.exists isset fls then f x else ()

let branch fl (f, g) = if isset fl then f else g

let show f = "flag " ^ f.name ^ " = " ^ if f.value then "true" else "false"
