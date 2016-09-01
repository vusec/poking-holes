open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util
open Util

type t = {
  name : string;
  (*
   * Max amount of bytes that might remain unallocated in
   * this hole; this value will keep getting reduced, either
   * when an allocation succeeds or when it fails.
   * The number of bytes is inclusive; i.e. if max is 4096,
   * that means that 4096 more bytes /could/ be allocatable.
   * NOTE NOTE NOTE: the actual size of the hole will most
   * likely be smaller.
   *)
  max : big_int;

  (* Amount of bytes we've allocated from this hole so far *)
  total : big_int;
} with sexp

let verify_pagemult descr num =
  let rem = num %% pagesize in
  if compare_big_int rem bi0 <> 0 then
    let s = sprintf "Tried to set %s to %s, which is not a multiple of \
                       the page size" descr (string_of_big_int num) in
    failwith s

let create name ~max =
  verify_pagemult "max" max;
  { name; max; total = bi0 }

let set_max t nmax =
  verify_pagemult "max" nmax;
  {t with max = nmax}

let set_total t ntotal =
  verify_pagemult "total" ntotal;
  {t with total = ntotal}

let to_s t =
  sprintf "{hole<%s>: max %s, total %s}" t.name
    (color ~color:`Yellow (humsize t.max)) (humsize t.total)

let namecmp h1 h2 = String.compare h1.name h2.name
