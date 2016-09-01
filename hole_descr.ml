open Core.Std
open Sexplib_num.Std.Big_int

type t = {
  name : string;
  behavior : Hole_of_interest.allocation_behavior;
  min : big_int;
  max: big_int;
} with sexp
