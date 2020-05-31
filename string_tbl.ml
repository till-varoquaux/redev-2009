open StdLabels
open MoreLabels

(**
   Very simple Hash consing. This module doesn't use weak pointers so it should
   be used in carefully.
*)
module Cons : sig
  type  'a tbl

  type 'a t =  private 'a

  val create : int -> 'a tbl
  val cons : 'a tbl -> 'a -> 'a t
  external uncons : 'a t -> 'a = "%identity"
end  = struct

  type 'a tbl = 'a list array
  type 'a t = 'a

  let create len = Array.create len []

  let cons tbl s =
    let bucket = Hashtbl.hash s mod Array.length tbl in
    let vals = tbl.(bucket) in
    let rec aux = function
      | [] -> raise Not_found
      | h::t ->
          if h = s then
            h
          else
            aux t
    in
    try
      aux vals
    with Not_found ->
      tbl.(bucket) <- s::vals;
      s

  external uncons : 'a t -> 'a = "%identity"
end

let tbl = Cons.create 17

let _s = Cons.cons tbl "1234"
let _ = Cons.uncons (Cons.cons tbl "1234") == (Cons.uncons  _s)
