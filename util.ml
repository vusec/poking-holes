open Core.Std
open Sexplib_num.Std.Big_int
open Printf
open Big_int_util

let opt_color = ref true
let color ~color str =
  if !opt_color then begin
    Core_extended.Color_print.color ~color str
  end else begin
    str
  end

let fmtsz size =
  let magnitudes = [
    (big_int_of_int (1024 * 1024 * 1024), "GB");
    (big_int_of_int (1024 * 1024), "MB");
    (big_int_of_int 1024, "KB");
  ] in
  let try_magn (magn, name) =
    let div = size %/ magn in
    if div %> bi0 then
      let r = (float_of_big_int size) /. (float_of_big_int magn) in
      Some (sprintf "%.2f%s" r name)
    else
      None
  in
  match List.find_map ~f:try_magn magnitudes with
    | Some str -> str
    | None -> sprintf "%sB" (string_of_big_int size)

(* Size in both formats *)
let humsize size =
  sprintf "%s (%s)" (fmtsz size) (string_of_big_int size)

let hex_of_bi bi = bi |> int64_of_big_int |> (sprintf "%#Lx")
