open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util

type hole_descr_by_pos =
  | AbsPosAndSortSize of int * int * Hole_descr.t list
  | BeforeStack of Hole_descr.t
  | EndOfUserspace of Hole_descr.t
with sexp

module HD = Hole_descr
module Hoi = Hole_of_interest

(* Interpolated after 10000 runs *)
let minA = (big_int_of_int 130044) %* bi1GiB
let maxA = (big_int_of_int 131068) %* bi1GiB

let simple_holes =
  [
    AbsPosAndSortSize
      (0, 1,
       [{
         HD.name = "A";
         HD.behavior = Hoi.AllocHigh;
         HD.min = minA;
         HD.max = maxA;
       }]);
    BeforeStack {
      HD.name = "B";
      HD.behavior = Hoi.AllocLow;
      HD.min = bi1GiB;
      HD.max = (big_int_of_int 1028) %* bi1GiB;
    };
    EndOfUserspace {
      HD.name = "C";
      HD.behavior = Hoi.AllocLow;
      HD.min = pagesize;
      HD.max = bi4 %* bi1GiB;
    };
  ]

let hidden_obj_size = bi2 %* bi1MiB

let page_align x =
  let rem = x %% pagesize in
  x %- rem

let hidden_object_holes =
  [
    AbsPosAndSortSize
      (0, 2,
       [
         (* Notice that small.max > large.min *)
         {
           HD.name = "Large";
           HD.behavior = Hoi.AllocHigh;
           HD.min = page_align ((minA %- hidden_obj_size) %/ bi2) %- pagesize;
           HD.max = maxA %- hidden_obj_size %- pagesize;
         };
         {
           HD.name = "Small";
           HD.behavior = Hoi.AllocHigh;
           HD.min = pagesize;
           HD.max = page_align ((maxA %- hidden_obj_size) %/ bi2) %+ pagesize;
         };
       ]);
    BeforeStack {
      HD.name = "B";
      HD.behavior = Hoi.AllocLow;
      HD.min = bi1GiB;
      HD.max = (big_int_of_int 1028) %* bi1GiB;
    };
    EndOfUserspace {
      HD.name = "C";
      HD.behavior = Hoi.AllocLow;
      HD.min = pagesize;
      HD.max = bi4 %* bi1GiB;
    };
  ]

type t = {
  split : int;
  mshs : big_int;
  preallocations : Preallocations.t sexp_option;
  hole_descrs_by_pos : hole_descr_by_pos list with default(simple_holes);
} with sexp

(*
 * Unfortunately, there's other gaps in a process's virtual address
 * space than just holeA, holeB and holeC. These gaps seem to normally be <10MB
 * in size, but the user can experiment with other values too.
 *)
let def_mshs = bi10 %* bi1MiB

let of_params
    ?(split=8)
    ?(mshs=def_mshs)
    ?(preallocations=None)
    ?(hole_descrs_by_pos = simple_holes)
    () =
  {
    split;
    mshs;
    preallocations;
    hole_descrs_by_pos;
  }
