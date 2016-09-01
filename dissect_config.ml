open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util

module Hoi = Hole_of_interest

type primitive =
  | MmapMunmap
  | Talloc
with sexp

type t = {
  livecmd : string;
  primitive : primitive;
  last_sanity_check_allocation : bool;
  resolver : string option;
  preallocations : Preallocations.t option;
  hole_descrs_by_size : Hole_descr.t list;
} with sexp

module HD = Hole_descr

let of_params ?(livecmd="./stub-pie")
    ?(primitive=MmapMunmap)
    ?(last_sanity_check_allocation=true)
    ?(resolver=None)
    ?(preallocations=None)
    ?(hole_descrs_by_size = [
      {
        HD.name = "A";
        HD.behavior = Hoi.AllocHigh;
        HD.max = big_int_of_string "140733193388032";
        HD.min = big_int_of_string "139633681760256";
      };])
    () =
  {
    livecmd;
    primitive;
    last_sanity_check_allocation;
    resolver;
    preallocations;
    hole_descrs_by_size;
  }
