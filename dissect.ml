open Core.Std
open Sexplib_num.Std.Big_int
open Printf
open Big_int_util
open Util

let dprintf = Debug.eprintf

let config = ref (Dissect_config.of_params ())

(* XXX: dup *)
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
  val alloc : t -> ?shadow:bool -> big_int -> unit -> (big_int, Error.t) Result.t
  val munmap : t -> ?shadow:bool -> addr:big_int -> size:big_int -> unit -> t
  val mmap_fixed : t -> ?shadow:bool -> addr:big_int -> size:big_int -> unit -> t
  val talloc : t -> big_int -> unit -> t option
  val vma_overlap : vma -> low:big_int -> high:big_int -> big_int
  val maps : t -> unit -> string
  val pid : t -> unit -> (int, Error.t) Result.t
  val initial_size_for_hole : t -> string -> big_int
  val holes_of_interest : t -> Hole_of_interest.t list
end

module type Primitive =
sig
  module Stub : Stub
  val talloc : Stub.t -> big_int -> Stub.t option
end

module MmapMunmapPrimitive(Stub : Stub) =
struct
  module Stub = Stub
  let talloc stub size =
    let res = Stub.alloc stub size () in
    match res with
    | Result.Ok addr when addr %> bi0 ->
       begin
         dprintf "=> Stub.alloc %s -> OK" (humsize size);
         Some (Stub.munmap stub ~addr ~size ())
       end
    | _ ->
       begin
         dprintf "=> Stub.alloc %s -> fail" (humsize size);
         None
       end
end

module TallocPrimitive(Stub : Stub) =
struct
  module Stub = Stub
  let talloc stub size = Stub.talloc stub size ()
end

module Strategy(Primitive : Primitive) =
struct
  module Stub = Primitive.Stub

  let rec binsearch stub ~low ~high =
    let s = sprintf "binsearch %s %s" (humsize (Pages.to_bytes low))
            (humsize (Pages.to_bytes high)) in
    dprintf "%s" (color ~color:`Green s);
    if Pages.equal high low then begin
      low
    end else begin
      if Pages.equal (Pages.sub high low) Pages.one then begin
        dprintf "%s low = %s, high = %s\nTrying high"
          (color ~color:`Red "Endgame!")
          (humsize (Pages.to_bytes low)) (humsize (Pages.to_bytes high));
        match Primitive.talloc stub (Pages.to_bytes high) with
        | Some _ ->
           high
        | None ->
           if (!config).last_sanity_check_allocation then begin
             dprintf "Trying low";
             match Primitive.talloc stub (Pages.to_bytes low) with
             | Some _ ->
                low
             | None ->
                (* It HAS to be low. The allocation was just a sanity check *)
                failwith "Last low allocation unexpectedly failed"
           end else begin
             low
           end
      end else begin
        let mid = Pages.div (Pages.add high low) Pages.two in
        match Primitive.talloc stub (Pages.to_bytes mid) with
        | Some stub ->
           binsearch stub ~low:mid ~high
        | None ->
           binsearch stub ~low ~high:(Pages.sub mid Pages.one)
      end
    end

  let deduce stub ~save_maps_to_path hole_descr =
    let initial_maps = Stub.maps stub () in
    ignore(match save_maps_to_path with
    | None -> ()
    | Some path -> Out_channel.write_all path ~data:initial_maps);
    let module HD = Hole_descr in
    let max = hole_descr.HD.max in
    let min = hole_descr.HD.min in
    let max = Pages.of_bytes max in
    let min = Pages.of_bytes min in
    let recovered_size = binsearch stub ~low:min ~high:max in
    let recovered_size = Pages.to_bytes recovered_size in
    printf "Recovered size for %s: %s\n%!" hole_descr.HD.name (humsize recovered_size);
    recovered_size

end

module type PrimitiveSig = module type of TallocPrimitive

let selprim primitive =
  let open Dissect_config in
  match primitive with
  | MmapMunmap -> (module MmapMunmapPrimitive : PrimitiveSig)
  | Talloc -> (module TallocPrimitive : PrimitiveSig)

let force_new_heap_allocation alloc stub =
  (* Force an ultra-large allocation that will fail, but glibc
   * will allocate its extra arena.
   * NOTE: the extra arena gets allocated at a 64MB-aligned address.
   * That means that we don't know the distance of this new heap
   * arena to the stuff we care about. Luckily though, the size of
   * the arena is stored at a known location, so we should be able
   * to get it if we have an arbitrary read. In the worst case (i.e.
   * no arbitrary read), we'd be stuck with 64MB - 4K of uncertainty.
   *)
  match alloc stub bi1PiB () with
  | Some _ -> failwith "1PiB allocation succeeded?!?"
  | None -> stub


module ResultChecker(Stub : Stub) =
struct
  let check stub recovered_size hdescr =
    let sizeA = Stub.initial_size_for_hole stub hdescr.Hole_descr.name in
    if recovered_size %= sizeA then begin
      printf "Correct\n%!";
      true
    end else begin
      printf "Wrong: recovered %s, actual %s\n%!" (humsize recovered_size)
        (humsize sizeA);
      false
    end
end

let doit load_from save_results_to_path save_maps_to_path configpath () =
  let start_time = Time.now () in
  begin
    match configpath with
    | Some path -> config := Dissect_config.t_of_sexp (Sexp.load_sexp path)
    | None -> ()
  end;
  dprintf "Using config:\n%s" (Sexp.to_string_hum (Dissect_config.sexp_of_t !config));
  Random.self_init ();
  let module Primitive = (val (selprim (!config).primitive) : PrimitiveSig) in
    match load_from with
    | None ->
       let module Primitive = Primitive(Live_stub) in
       let module RC = ResultChecker(Live_stub) in
       let module StubUtil = Stub_util.Make(Live_stub) in
       let module Strategy = Strategy(Primitive) in
       let gih = Live_stub.get_interesting_holes_by_size (!config).hole_descrs_by_size in
       let stub = Live_stub.create gih (!config).livecmd in
       let stub = force_new_heap_allocation Live_stub.talloc stub in
       let stub = StubUtil.perform_preallocations stub (!config).preallocations in

       let do_hole (stub, results) hdescr =
         let start_nops = Live_stub.n_operations stub in
         printf "%s\n%!" (color ~color:`Yellow (sprintf "Trying to find the size of hole '%s'" hdescr.Hole_descr.name));
         let recovered_size = Strategy.deduce stub ~save_maps_to_path hdescr in
         let recovered_size = StubUtil.further_resolve_size stub (!config).resolver recovered_size in
         if not (RC.check stub recovered_size hdescr) then begin
           failwith (sprintf "Failed to pinpoint hole '%s'" hdescr.Hole_descr.name)
         end;
         match Live_stub.alloc ~shadow:false stub recovered_size () with
         | Result.Ok addr when not (addr %= bi0) ->
            let module DR = Dissect_results in
            let end_nops = Live_stub.n_operations stub in
            let res = {
              DR.name = hdescr.Hole_descr.name;
              recovered_size;
              DR.noperations = end_nops - start_nops;
            } in
            (stub, res :: results)
         | _ ->
            failwith "Couldn't allocate discovered hole"
       in
       let _, results = List.fold_left ~init:(stub, []) ~f:do_hole
         (!config).hole_descrs_by_size
       in
       begin
         match save_results_to_path with
         | None -> ()
         | Some path ->
            begin
              let res = {
                Dissect_results.results = results;
                Dissect_results.runtime = Time.diff (Time.now ()) start_time;
              } in
              Sexp.save_hum path (Dissect_results.sexp_of_result res)
            end
       end
    | Some _ ->
       (*
        * This is broken with talloc. Needs
        * a) a shadow stub that emulates the posix_memalign behavior
        * b) a shadow stub that emulates the new_heap glibc behavior
        * c) a shadow stub that saves all process memory so that we
        *    can resolve the size of the heap_area :(
        *)
       failwith "TBD"
(*
       let module Primitive = Primitive(Shadow_stub) in
       let mapsstr = In_channel.read_all path in
       let stub = Shadow_stub.create mapsstr in
       let module Strategy = Strategy(Primitive) in
       let hdescr = List.hd_exn (!config).hole_descrs_by_size in
       let recovered_size = Strategy.deduce stub ~save_results_to_path ~save_maps_to_path hdescr in
       let module RC = ResultChecker(Shadow_stub) in
       let recovered_size = RC.further_resolve_size stub (!config).resolver recovered_size in
       RC.check stub recovered_size
*)

let command =
  Command.basic
    ~summary:"Deduce the potential size of holeA"
    Command.Spec.(
      empty
      +> flag "-l" (optional string) ~doc:"PATH Load initial memory configuration from path"
      +> flag "-r" (optional string) ~doc:"PATH Save results to path"
      +> flag "-s" (optional string) ~doc:"PATH Save initial memory configuration in path"
      +> flag "-c" (optional string) ~doc:"PATH Config file"
    ) doit

let () =
  dprintf "Starting up";
  Command.run ~version:"0.1" command
