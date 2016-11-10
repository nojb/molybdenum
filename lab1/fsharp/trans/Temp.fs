(* L1 Compiler
 * Temporaries
 * Translated from SML source by bhamme
 *)

namespace Lab1.Trans

open Lab1.Util

module Temp =
    type temp = int

    let counter = ref 1

    (* warning: calling reset() may jeopardize uniqueness of temps! *)
    let reset () = ( counter := 1 )
    let create () =
        let retcounter = !counter
        counter := !counter + 1
        retcounter
    
    let name ( t: int ) = sprintf "%%t%d" t

    let compare (t1,t2) = Order.compareToZero(t1 - t2)
