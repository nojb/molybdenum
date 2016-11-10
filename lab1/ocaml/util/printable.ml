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
    val find' : key -> 'a t -> 'a option
    val format : (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit
  end

let rec format_list fmt ff = function
  | [] -> ()
  | [v] -> Format.fprintf ff "%a" fmt v
  | v::tl -> Format.fprintf ff "%a,@ %a" fmt v (format_list fmt) tl


module MakeSet (P : POrderedType) =
  struct
    include Set.Make(P)

    let format ff s =
      Format.fprintf ff "@[<4><%a>@]" (format_list P.format) (elements s)
  end

module MakeMap (P: POrderedType) =
  struct
    include Map.Make(P)

    let find' k m =
      try Some (find k m) with Not_found -> None

    let format vf ff m =
      let rec format' ff = function
        | [] -> ()
        | [(key, value)] ->
            Format.fprintf ff "@[<4>%a =>@ %a@]"
              P.format key vf value
        | (key, value)::rest ->
            Format.fprintf ff "@[<4>%a =>@ %a@],@ %a"
              P.format key vf value format' rest in
      Format.fprintf ff "@[<4>%a]" format' (bindings m)
  end
