include Stub_base
open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util
open Util

let dprintf = Debug.eprintf

type t = {
  n_operations : int ref;
  channels : Unix.Process_channels.t;
  interesting_holes : Hole_of_interest.t SM.t;
  interesting_holes_parser : string -> Hole_of_interest.t list;
  shadow : Shadow_stub.t;
}

let initial_size_for_hole stub name =
  match SM.find stub.interesting_holes name with
    | None -> failwith (sprintf "Couldn't find hole %s in map" name)
    | Some hole -> hole.high %- hole.low

let n_operations stub =
  let shadow_nops = Shadow_stub.n_operations stub.shadow in
  if shadow_nops <> !(stub.n_operations) then begin
    let s = sprintf "n_operations differ between live (%d) and shadow (%d)"
      !(stub.n_operations) shadow_nops in
    dprintf "%s" s
  end;
  !(stub.n_operations)

let get_response channels () =
  let open Unix.Process_channels in
  try
    dprintf "get_response1";
    let line1 =  input_line channels.stdout in
    let line1 = String.rstrip line1 in
    dprintf "get_response2";
    let line2 = input_line channels.stdout in
    dprintf "get_response: done";
    let line2 = String.rstrip line2 in
    if String.equal line1 "OK" then begin
      Result.Ok line2
    end else begin
      Or_error.error_string line2
    end
  with  End_of_file ->
    dprintf "stderr of stub:";
    dprintf "%s" (In_channel.input_all channels.stderr);
    Or_error.error_string "Stub process died"

let do_pid channels =
  let open Unix.Process_channels in
  fprintf channels.stdin "pid\n%!";
  match get_response channels () with
    | Result.Ok pid -> Result.Ok (int_of_string pid)
    | Result.Error _ as err -> err

let pid {channels; _} () = do_pid channels

let do_maps channels =
  let mapspath = sprintf "/proc/%d/maps" in
  match do_pid channels with
    | Result.Error err ->
      failwith (sprintf "Can't get maps: %s" (Error.to_string_hum err))
    | Result.Ok pid ->
      let ic = Unix.open_process_in (sprintf "./maps %s" (mapspath pid)) in
      let str = In_channel.input_all ic in
      In_channel.close ic;
      str
let maps t () = do_maps t.channels

let diff_AB_sizes ~prev ~curr =
  let diffs = match List.zip prev curr with
    | None ->
      let s = sprintf "hole size lists have different lengths: prev %d curr %d"
        (List.length prev) (List.length curr) in
      failwith s
    | Some pairs ->
      List.map ~f:(fun ((pname, psize), (cname, csize)) ->
        assert (String.equal pname cname);
        (pname, psize %- csize)) pairs
  in
  match List.filter ~f:(fun (_, diff) -> diff %> bi0) diffs with
    | [(hole_name, _)] ->
      sprintf "Allocation came from %s" hole_name
    | [] ->
      "allocation unexpectedly failed!"
    | more ->
      let s = List.map ~f:(fun (name, diff) ->
        sprintf "hole %s diff %s" name (string_of_big_int diff)) more in
      let s = (String.concat ~sep:", " s) in
      failwith (sprintf "Allocation came from >1 hole: %s" s)

let alloc stub ?(shadow=true) size () =
  stub.n_operations := !(stub.n_operations) + 1;
  dprintf "Stub.alloc %s" (humsize size);
  let open Unix.Process_channels in
  let mapsstr = maps stub () in
  let prev_hole_sizes = actual_hole_sizes stub.interesting_holes mapsstr in
  fprintf stub.channels.stdin "alloc %Ld\n%!" (int64_of_big_int size);
  match get_response stub.channels () with
    | Result.Ok line ->
      begin
        match String.split ~on:' ' line with
          | [addr; _] ->
            let addr = big_int_of_string addr in
            if addr %> bi0 then begin
              let mapsstr = maps stub () in
              let curr_hole_sizes = actual_hole_sizes stub.interesting_holes mapsstr in
              dprintf "%s" (diff_AB_sizes ~prev:prev_hole_sizes
                              ~curr:curr_hole_sizes);
            end;
            if shadow then begin
              ignore(match Shadow_stub.alloc stub.shadow size () with
              | Result.Ok shadow_addr ->
                 if not (shadow_addr %= addr) then begin
                   let s = sprintf "Shadow addr (%s) does not match actual addr (%s)"
                     (hex_of_bi shadow_addr) (hex_of_bi addr) in
                   dprintf "maps before\n%s"mapsstr;
                   dprintf "Shadow_stub maps:\n%s" (Shadow_stub.maps stub.shadow ());
                   failwith s
                 end
              | Result.Error err ->
                 failwith (sprintf "Shadow_stub.alloc failed: %s" (Error.to_string_hum err)))
            end;
            Result.Ok addr
          | _ ->
            let s = sprintf "Did not get exactly two \
                    fields from the stub: %s"
              line in
            failwith s
      end
    | Result.Error _ as err -> err

let munmap stub ?(shadow=true) ~addr:biaddr ~size:bisize () =
  stub.n_operations := !(stub.n_operations) + 1;
  let addr = int64_of_big_int biaddr in
  dprintf "Stub.munmap %#Lx %s" addr (humsize bisize);
  let size = int64_of_big_int bisize in
  fprintf stub.channels.stdin "munmap %Ld %Ld\n%!" addr size;
  match get_response stub.channels () with
  | Result.Ok line ->
     if not (String.equal line "Unmapped") then begin
       failwith (sprintf "Unexpected response from stub: %s" line)
     end;
    if shadow then begin
      {stub with
        shadow = Shadow_stub.munmap stub.shadow ~addr:biaddr ~size:bisize ()}
    end else begin
      stub
    end
  | Result.Error err ->
     failwith (sprintf "Munmap failed: %s" (Error.to_string_hum err))

let talloc stub bisize () =
  stub.n_operations := !(stub.n_operations) + 1;
  let size = int64_of_big_int bisize in
  dprintf "talloc: live maps on entry: %s" (maps stub ());
  fprintf stub.channels.stdin "talloc %Ld\n%!" size;
  match get_response stub.channels () with
  | Result.Ok _ ->
     begin
       match Shadow_stub.talloc stub.shadow bisize () with
       | Some shadow ->
          dprintf "talloc: live maps after success: %s" (maps stub ());
          Some {stub with shadow}
       | None ->
          failwith "Live talloc succeeded, shadow failed"
     end
  | Result.Error err ->
     begin
       dprintf "talloc error: %s" (Error.to_string_hum err);
       match Shadow_stub.talloc stub.shadow bisize () with
       | Some _ ->
          dprintf "Live talloc failed, shadow succeeded";
         (* XXX: because of the heap allocation by glibc, there's an additional
            allocation not duplicated in the shadow stub. Ignore for now *)
         None
       | None ->
          dprintf "talloc: live maps after failure: %s" (maps stub ());
          None
     end

let mmap_fixed stub ?(shadow=true) ~addr ~size () =
  let addr64 = int64_of_big_int addr in
  let size64 = int64_of_big_int size in
  dprintf "Stub.mmap_fixed %#Lx %s\n" addr64 (humsize size);
  fprintf stub.channels.stdin "mmap_fixed %#Lx %Ld\n%!" addr64 size64;
  match get_response stub.channels () with
  | Result.Ok _ ->
     begin
       if shadow then begin
         let shadow = Shadow_stub.mmap_fixed stub.shadow ~addr ~size () in
         {stub with shadow}
       end else begin
         stub
       end
     end
  | Result.Error err ->
     failwith (sprintf "mmap_fixed failed: %s" (Error.to_string_hum err))

let quit {channels; _} () =
  dprintf "Stub.quit";
  let open Unix.Process_channels in
  fprintf channels.stdin "quit\n%!";
  match get_response channels () with
    | Result.Ok _ -> Result.Ok ()
    | Result.Error _ as err -> err


let refresh_interesting_holes stub () =
  let mapsstr = do_maps stub.channels in
  let interesting_holes = do_get_interesting_holes mapsstr
    stub.interesting_holes_parser
  in
  {stub with interesting_holes}

let create gih cmd =
  let channels = Unix.open_process_full cmd ~env:(Array.empty ()) in
  let mapsstr = do_maps channels in
  let interesting_holes = do_get_interesting_holes mapsstr gih in
  let shadow = Shadow_stub.create gih mapsstr in
  {n_operations = ref 0; channels; interesting_holes; interesting_holes_parser = gih; shadow}

let holes_of_interest stub = SM.data stub.interesting_holes

let lowest_gap stub () =
  match get_gaps (maps stub ()) with
  | gap :: _ -> gap
  | [] -> failwith "Live_stub: No lowest gap!"
