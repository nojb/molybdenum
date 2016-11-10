namespace Lab1.Trans
  module Temp = begin
    type temp = int
    val counter : int ref
    val reset : unit -> unit
    val create : unit -> int
    val name : int -> string
    val compare : int * int -> Util.Order.order
  end

