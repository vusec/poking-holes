open Core.Std
open Sexplib_num.Std.Big_int
open Printf
open Big_int_util
open Util

let dprintf = Debug.eprintf

module type Stub =
sig
  type vma = {
    low : big_int;
    (*
     * The high bound is not inclusive; i.e. a vma might be
     * 0x1000-0x2000 which means that the vma starts at 0x1000
     * and 0x2000 is part of a different vma/page. This way,
     * the size of the vma is always high - low.
     *)
    high : big_int;
    flags : string;
    name : string;
  }
  type t
  val mmap_fixed : t -> ?shadow:bool -> addr:big_int -> size:big_int -> unit -> t
  val vma_overlap : vma -> low:big_int -> high:big_int -> big_int
  val pid : t -> unit -> (int, Error.t) Result.t
  val refresh_interesting_holes : t -> unit -> t
  val lowest_gap : t -> unit -> vma
end

module Make(Stub : Stub) =
struct
  let do_preallocations ~shadow stub preallocations =
    let open Dissect_config in
    let open Preallocations in
    let width = preallocations.high %- preallocations.low in
    let high_bound = (Stub.lowest_gap stub ()).high in
    let rec do_alloc vmas stub size count =
      let offset = Random.int64 (int64_of_big_int (width %- size)) in
      let offset = big_int_of_int64 offset in
      let offset = offset %- (offset %% pagesize) in
      let low = preallocations.low %+ offset in
      let high = preallocations.low %+ offset %+ size in
      if (List.exists ~f:(fun vma ->
        Stub.vma_overlap vma ~low ~high %> bi0) vmas) ||
        (high %> high_bound)
      then begin
        (* Just retry *)
        alloc vmas stub size (count - 1)
      end else begin
        let vma = {Stub.low = low; Stub.high = high; Stub.flags = ""; Stub.name = "[preallocation]";} in
        let stub = Stub.mmap_fixed stub ~shadow ~addr:low ~size () in
        dprintf "Preallocated %s @ %#Lx" (humsize size) (int64_of_big_int low);
        (stub, vma :: vmas)
      end
    and alloc vmas stub size count =
      if count = 0 then begin
        failwith "Couldn't prealloc area!"
      end;
      do_alloc vmas stub size count
    in
    let stub, _ = List.fold_left ~init:(stub, []) ~f:(fun (stub, vmas) size ->
      alloc vmas stub size 10) preallocations.sizes
    in
    stub

  let perform_preallocations ?(shadow=true) stub preallocations =
    match preallocations with
    | None ->
       stub
    | Some preallocations ->
       let stub = do_preallocations stub ~shadow preallocations in
       Stub.refresh_interesting_holes stub ()

  let further_resolve_size stub resolver recovered_size =
    match resolver with
    | None ->
       recovered_size
    | Some cmd ->
       let pid = match Stub.pid stub () with
         | Result.Ok pid ->
            pid
         | Result.Error err ->
            failwith (sprintf "Stub.pid failed: %s" (Error.to_string_hum err))
       in
       (* The resolver is called as "cmd $pid $hole_size" *)
       let recovered_size = string_of_big_int recovered_size in
       let cmd = sprintf "%s %d %s" cmd pid recovered_size in
       dprintf "EXEC: %s" cmd;
       let ic = Unix.open_process_in cmd in
       let output = In_channel.input_all ic in
       match Unix.Exit_or_signal.or_error (Unix.close_process_in ic) with
       | Result.Ok () ->
          dprintf "Resolver says %s" output;
          big_int_of_string output
       | Result.Error err ->
          let err = Error.to_string_hum err in
          let s = sprintf "Error resolving in pid %d with size %s: %s" pid
            recovered_size err in
          failwith s

end
