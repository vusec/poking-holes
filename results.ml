open Core.Std
open Sexplib_num.Std.Big_int
open Util

module Hoi = Hole_of_interest

type initial_size = {
  name : string;
  size : big_int;
} with sexp

type result = {
  interesting_holes : Hoi.t list;
  n_operations : int;
  states : States.t;
  runtime : Time.Span.t;
} with sexp

let hoi_names res = List.map ~f:(fun hoi -> hoi.Hoi.name) res.interesting_holes

let summarize res =
  let uncertainty_hole h =
    let unc = States.total_maxes res.states h.Hoi.name in
    printf "sum(max(%s)) over all states: %s\n" h.Hoi.name (humsize unc)
  in
  printf "runtime = %s\n" (Time.Span.to_short_string res.runtime);
  printf "n_operations = %d\n" res.n_operations;
  List.iter ~f:uncertainty_hole res.interesting_holes;
  let consistent = match States.check_sizes res.states res.interesting_holes with
    | Ok _ -> true
    | Error _ -> false
  in
  printf "All actual hole sizes included in the final states? %b\n" consistent
