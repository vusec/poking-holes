open Core.Std
open Sexplib_num.Std.Big_int

type maximum_hole_size = {
  value : big_int;
  multiplicity : int;
}

type state = {
  holes : Hole.t list;
} with sexp

val state_to_s : state -> string
val state_nholes : state -> int
val state_sorted_maxes : state -> maximum_hole_size list
val state_of_hole_descrs : Hole_descr.t list -> state

val find_hole : name:string -> state -> Hole.t

type t = private {
  states : state list;
  nholes : int; (* Number of holes in each state *)
} with sexp

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
val create : state list -> t
val multimap : (state -> state list) -> t -> t
val nholes : t -> int
val max_hole_sizes : t -> maximum_hole_size list
val total_maxes : t -> string -> big_int
val check_sizes : t -> Hole_of_interest.t list -> unit list Or_error.t
val to_s : t -> string
val nstates : t -> int
