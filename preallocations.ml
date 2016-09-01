open Core.Std
open Sexplib_num.Std.Big_int

type t = {
  sizes : big_int list;
  low : big_int;
  high : big_int;
} with sexp
