open Core.Std
open Sexplib_num.Std.Big_int
open Printf
open Big_int_util
open Util

type t = big_int

let of_bytes bytes =
  if not ((bytes %% pagesize) %= bi0) then begin
    let s = sprintf "%s bytes not an integral number of pages"
      (humsize bytes) in
    failwith s
  end;
  bytes %/ pagesize

let to_bytes pages = pages %* pagesize

let add = add_big_int
let sub = sub_big_int
let equal = eq_big_int
let div = div_big_int
let one = bi1
let two = bi2
