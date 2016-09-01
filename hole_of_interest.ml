open Core.Std
open Sexplib_num.Std.Big_int

type allocation_behavior =
  | AllocHigh
  | AllocLow with sexp

type t = {
  name : string;
  low : big_int;
  (* Not inclusive. I.e. hole is 0x0-0x1000 *)
  high : big_int;
  behavior : allocation_behavior;
} with sexp

let to_s hoi =
  Sexp.to_string (sexp_of_t hoi)
