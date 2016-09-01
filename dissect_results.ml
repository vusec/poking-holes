open Core.Std
open Sexplib_num.Std.Big_int

type hole_result = {
  name : string;
  recovered_size : big_int;
  noperations : int;
} with sexp

type result = {
  results : hole_result list;
  runtime : Time.Span.t;
} with sexp
