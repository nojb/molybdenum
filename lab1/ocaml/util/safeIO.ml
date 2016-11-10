(* L1 Compiler
 * Safe(r) I/O functions
 * Author: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)


type 'a result = Value of 'a | Exception of exn

(* Call f x with the guarantee that prot x will be called afterward *)
let unwind always f x =
  let result = try Value (f x) with e -> Exception e in
  let () = always x in
  match result with
    Value x -> x
  | Exception e -> raise e


let withOpenIn filename scope =
  unwind close_in scope (open_in filename)

let withOpenOut filename scope =
  unwind close_out scope (open_out filename)
