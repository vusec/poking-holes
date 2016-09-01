open Core.Std
open Sexplib_num.Std.Big_int
open Printf
open FileUtil
open Stats

module Hoi = Hole_of_interest

let dprintf = Debug.eprintf

type result_stats = {
  uncertainties : BigIntStats.t String.Table.t;
  nstates : IntStats.t;
  noperations : IntStats.t;
}

let get_testruns dir =
  let results = find (And (Is_file, Basename_is "results")) dir
    (fun acc path ->
      (FilePath.dirname path) :: acc
    ) [] in
  results

let rpath_of_testrun path =
  let resultspath = sprintf "%s/results" path in
  Sexp.load_sexp resultspath

let add_res_to_stats stats res =
  List.iter ~f:(fun h ->
    let u = States.total_maxes res.states h.Hoi.name in
    String.Table.change stats.uncertainties h.Hoi.name (function
    | None -> failwith (sprintf "No stats for hole %s" h.Hoi.name)
    | Some uncertainty -> Some (BigIntStats.add_value uncertainty u)))
    res.Results.interesting_holes;
  {stats with
    nstates = IntStats.add_value stats.nstates (States.nstates res.Results.states);
    noperations = IntStats.add_value stats.noperations res.Results.n_operations;
  }


(* Generate a TSV for all collected values *)
let do_dump stats =
  let box = List.map ~f:(fun x -> [x]) in
  let paste l1 l2 = List.map ~f:(fun (a, b) -> List.append a b) (List.zip_exn l1 l2) in
  let values = [
    "nstates" :: (IntStats.str_values stats.nstates);
    "noperations" :: (IntStats.str_values stats.noperations);
  ] in
  let values = String.Table.fold ~init:values ~f:(fun ~key:name ~data:stats acc ->
    let list = (sprintf "uncertainty%s" name) :: (BigIntStats.str_values stats) in
    list :: acc) stats.uncertainties
  in
  let values = List.reduce_exn ~f:paste (List.map ~f:box values) in
  let values = List.map ~f:(String.concat ~sep:"\t") values in
  String.concat ~sep:"\n" values

let dump tsvpath stats =
  match tsvpath with
  | None ->
     ()
  | Some path ->
     Out_channel.write_all path ~data:(do_dump stats)

let doit tsvpath directory () =
  let resultpaths = get_testruns directory in
  match resultpaths with
  | [] ->
     failwith (sprintf "No results in %s" directory)
  | firstres :: _ ->
     begin
       let res_of_path = Fn.compose Results.result_of_sexp rpath_of_testrun in
       let firstres = res_of_path firstres in
       let alist = List.map ~f:(fun name ->
         let statname = sprintf "uncertainty%s" name in
         (name, BigIntStats.make statname)) (Results.hoi_names firstres) in
       let uncertainties = String.Table.of_alist_exn alist in
       let init = {
         uncertainties;
         nstates = IntStats.make "nstates";
         noperations = IntStats.make "noperations";
       } in
       let stats = List.fold_left ~f:(fun stats rpath ->
         let res = res_of_path rpath in
         add_res_to_stats stats res
       ) ~init resultpaths in
       dump tsvpath stats;
       String.Table.iter ~f:(fun ~key:name ~data:stat ->
         Printf.printf "%s\n" (BigIntStats.to_s stat)) stats.uncertainties;
       Printf.printf "%s\n" (IntStats.to_s stats.nstates);
       Printf.printf "%s\n" (IntStats.to_s stats.noperations)
     end

let command =
  Command.basic
    ~summary:"Summarize monparnes results"
    Command.Spec.(
      empty
      +> flag "-o" (optional string) ~doc:"PATH Dump stats in TSV for to path"
    +> anon ("filename" %: string))
    doit

let () =
  Command.run ~version:"0.1" command
