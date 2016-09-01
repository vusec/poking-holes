open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util

type maximum_hole_size = {
  value : big_int;
  multiplicity : int;
}

type state = {
  holes : Hole.t list;
} with sexp

let state_to_s state =
  let hstrs = List.map ~f:Hole.to_s state.holes in
  sprintf "[%s]" (String.concat ~sep:", " hstrs)

let state_max_cmp a b = compare_big_int b.Hole.max a.Hole.max

let state_nholes state = List.length state.holes

let state_sorted_maxes state =
  let sorted_holes = List.sort ~cmp:state_max_cmp state.holes in
  let sorted_maxes = List.map ~f:(fun h -> h.Hole.max) sorted_holes in
  let groups = List.group ~break:(fun a b -> not (a %= b)) sorted_maxes in
  List.map ~f:(function
  | [] -> failwith "empty group of state maxes"
  | hd :: tail -> {value = hd; multiplicity = List.length tail + 1}) groups

let state_of_hole_descrs hdescrs =
  let holes = List.map ~f:(fun h ->
    Hole.create h.Hole_descr.name ~max:h.Hole_descr.max) hdescrs
  in
  {holes}

let find_hole ~name state =
  match List.find ~f:(fun h -> String.equal h.Hole.name name) state.holes with
  | Some h ->
     h
  | None ->
     failwith (sprintf "Couldn't find hole %s in state %s" name (state_to_s state))

type t = {
  states : state list;
  nholes : int; (* Number of holes in each state *)
} with sexp

let create states =
  let nholes = match states with
    | [] -> 0
    | x :: rest ->
       let n = state_nholes x in
       List.iter ~f:(fun st ->
	 assert ((st |> state_nholes) = n)) rest;
       n
  in
  {states; nholes}

let multimap f t =
  let states = List.map ~f t.states in
  {t with states = List.concat states}

let nholes {nholes; _} = nholes

let max_hole_sizes {states; _} =
  let maxes = List.map ~f:state_sorted_maxes states in
  List.concat maxes

(* let sorted_maxes t = *)
(*   let cmp = state_max_cmp in *)
(*   (\* Get the maximally-sized hole of each state *\) *)
(*   let max_holes = List.map ~f:(fun st -> *)
(*     let sorted_holes_for_state = List.sort ~cmp st.holes in *)
(*     match sorted_holes_for_state with *)
(*     | [] -> failwith "zero holes for state!" *)
(*     | hole :: _ -> hole *)
(*   ) t.states in *)
(*   (\* Sort the above maxes *\) *)
(*   List.map ~f:(fun h -> h.Hole.max ) (List.sort ~cmp max_holes) *)

let total_maxes t hole_name =
  List.fold_left ~f:(fun acc state ->
    let holeA = find_hole ~name:hole_name state in
    acc %+ holeA.Hole.max) ~init:bi0 t.states

module Hoi = Hole_of_interest
let check_sizes t holes_of_interest =
  let check hoi =
    let size = hoi.Hoi.high %- hoi.Hoi.low in
    let state_includes_hole state =
      let hole = find_hole ~name:hoi.Hoi.name state in
      hole.Hole.total %<= size && size %<= (hole.Hole.total %+ hole.Hole.max)
    in
    match List.exists ~f:state_includes_hole t.states with
    | true ->
       Result.Ok ()
    | false ->
       let s = sprintf "%s is not included in the final states" (Hoi.to_s hoi) in
       Or_error.error_string s
  in
  Or_error.combine_errors (List.map ~f:check holes_of_interest)

let to_s t =
  let ss = List.map ~f:state_to_s t.states in
  let s = String.concat ~sep:"\n" ss in
  sprintf "[%d states] |%s|" (List.length t.states) s

let nstates t = List.length t.states
