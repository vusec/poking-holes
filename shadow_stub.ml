(*
 * This module pretends to be talking to the kernel but actually
 * just fakes an allocation and returns the address that the kernel
 * would return. It gets initialized with the contents of the
 * /proc/pid/maps file. The LiveStub always passes every allocation
 * request to the ShadowStub too and checks that the results match.
 *
 * However, it's also possible to start the program using a saved
 * maps file, in which case we only make use of the ShadowStub. This
 * way, we can duplicate a run of the algorithm on a previously observed
 * (random) address space layout. We use that to a) test out changes in
 * the algorithm against the exact same evaluation set (taking randomization
 * out of the picture) b) speed up repeated runs.
 *)

include Stub_base
open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util
open Util

let dprintf = Debug.eprintf

type t = {
  n_operations : int ref;
  vmas : [`Allocated of vma | `Gap of vma] list ref;
  interesting_holes : Hole_of_interest.t SM.t;
  interesting_holes_parser : string -> Hole_of_interest.t list;
}

let n_operations stub = !(stub.n_operations)

let consistency_check (vmas : [`Allocated of vma | `Gap of vma] list) =
  ignore(List.fold_left ~init:(None) ~f:(fun prev vma ->
    match prev with
      | None ->
        Some vma
      | Some prev ->
        begin
          if not (prev.high %= vma.low) then begin
            let s = sprintf "Unexpected gap between %s and %s"
              (vma_to_s prev) (vma_to_s vma) in
            failwith s
          end;
          Some vma
        end) (List.map ~f:unbox_vma vmas))

let create gih mapsstr =
  let vmas = get_vmas mapsstr in
  dprintf "ShadowStub.create: maps are:\n%s" mapsstr;
  consistency_check vmas;
  let interesting_holes = do_get_interesting_holes mapsstr gih in
  {n_operations = ref 0; vmas = ref vmas; interesting_holes; interesting_holes_parser = gih;}

let do_maps vmas =
  let s = String.concat ~sep:"\n" (List.map ~f:vma_to_mapsline vmas) in
  s ^ "\n"

let maps t () =
  do_maps !(t.vmas)

let make_vma ~low ~high = {name = "[stub_alloc]"; flags = "----"; low; high;}

let place_ascending_walk ~low ~high acc =
  high :: low :: acc

let place_descending_walk ~low ~high acc =
  low :: high :: acc

let do_alloc stub size place (acc, addr) vma =
    match addr, vma with
    | None, `Gap vma when (vma_size vma) %>= size ->
       begin
         dprintf "ShadowStub.alloc: picked gap %s" (vma_to_s vma);
         let hole = hole_of_interest_for_gap (SM.data stub.interesting_holes) vma in
         match hole.behavior with
         | Hoi.AllocHigh ->
            begin
              (* Adjust gap, add VMA on the 'high' side *)
              let ngap = {vma with high = vma.high %- size} in
              let addr = ngap.high in
              let nvma = make_vma ~low:addr ~high:vma.high in
              let acc = place ~low:(`Gap ngap) ~high:(`Allocated nvma) acc in
              (acc, Some addr)
            end
         | Hoi.AllocLow ->
            begin
              (* Adjust gap, add VMA on the 'low' side *)
              let ngap = {vma with low = vma.low %+ size} in
              let addr = vma.low in	(* prev, unadjusted low *)
              let nvma = make_vma ~low:vma.low ~high:ngap.low in
              let acc = place ~low:(`Allocated nvma) ~high:(`Gap ngap) acc in
              (acc, Some addr)
            end
       end
    | _ ->
       (vma :: acc, addr)

let locate_libc_so vmas =
  let libcre = Str.regexp ".*/libc-[2-9][.][1-9][0-9][.]so" in
  let res = List.findi ~f:(fun _ vma ->
    match vma with
    | `Allocated vma -> Str.string_match libcre vma.name 0
    | _ -> false) vmas
  in
  match res with
  | None -> failwith "Couldn't locate libc in the vmas"
  | Some (idx, _) -> idx

let alloc stub ?(shadow:_) size () =
  stub.n_operations := !(stub.n_operations) + 1;
  dprintf "Stub.alloc %s" (humsize size);
  let libc_idx = locate_libc_so !(stub.vmas) in
  (* OK, so we're assuming that the first gap below libc.so is where
   * mmap allocations will be satisfied from preferentially.
   *)
  let vmas_low, vmas_high = List.split_n !(stub.vmas) libc_idx in
  (* First, try to allocate from mmap_base downwards.
   * Notice: we walk the list in reverse order, hence we need to flip the
   * allocation behavior of the gaps.
   *)
  let vmas_low, addr = List.fold_left ~init:([], None) ~f:(fun (acc, addr) vma ->
    do_alloc stub size place_descending_walk (acc, addr) vma
  ) (List.rev vmas_low) in
  let vmas, addr =
    match addr with
    | None ->
       begin
         let vmas_high, addr = List.fold_left ~init:([], None) ~f:(fun (acc, addr) vma ->
           do_alloc stub size place_ascending_walk (acc, addr) vma
         ) vmas_high in
         let vmas_high = List.rev vmas_high in
         (List.append vmas_low vmas_high, addr)
       end
    | Some _ ->
       begin
         (List.append vmas_low vmas_high, addr)
       end
  in
  match addr with
  | None ->
     Result.Ok bi0
  | Some addr ->
     stub.vmas := vmas;
     Result.Ok addr

let dealloc_vma ~biaddr ~size vma =
  match vma with
  | `Gap vma when vma_includes vma biaddr ->
     failwith (sprintf "munmap addr %s is in gap %s"
                 (string_of_big_int biaddr) (vma_to_s vma))
  | `Gap _ ->
     vma
  | `Allocated vma when vma_includes vma biaddr ->
     if not (biaddr %= vma.low) then begin
       (* Given our current algo, this should never happen *)
       failwith (sprintf "Unimplemented: munmap of addr within a VMA (%s)"
                   (vma_to_s vma))
     end else begin
       if not (size %= (vma.high %- vma.low)) then begin
         (* Again, we don't expect to be doing this. If we get
          * here, it's very likely the caller has messed up.
          *)
         failwith (sprintf "Tried to do partial munmap of vma %s"
                     (vma_to_s vma))
       end;
       dprintf "dealloc_vma: changed vma %s to be a gap" (vma_to_s vma);
       `Gap vma
     end
  | `Allocated _ ->
     vma

let merge_adjacent_gaps (vmas : [`Allocated of vma | `Gap of vma] list) =
  let vmas = List.fold_left ~init:[] ~f:(fun acc curr ->
    match acc, curr with
    | (prev :: rest), `Gap vma2 ->
       begin
         match prev with
         | `Gap vma1 ->
            begin
              if not (vma1.high %= vma2.low) then begin
                let s = sprintf "adjacent gaps (%s) and (%s) not actually adjacent"
                  (vma_to_s vma1) (vma_to_s vma2) in
                failwith s
              end;
              let vma = {
                name = "";
                high = vma2.high;
                low = vma1.low;
                flags = "gap";
              } in
              dprintf "merging adjacent gaps: %s, %s to %s" (vma_to_s vma1)
                (vma_to_s vma2) (vma_to_s vma);
              (`Gap vma) :: rest
            end
         | _ ->
            curr :: acc
       end
    | _ ->
       curr :: acc) vmas
  in
  List.rev vmas

let munmap t ~addr:biaddr ~size () =
  t.n_operations := !(t.n_operations) + 1;
  let addr = int64_of_big_int biaddr in
  dprintf "Stub.munmap %#Lx %s" addr (humsize size);
  let vmas = List.map ~f:(dealloc_vma ~biaddr ~size) !(t.vmas) in
  let vmas = merge_adjacent_gaps vmas in
  t.vmas := vmas;
  t

let talloc t bisize () =
  let size = int64_of_big_int bisize in
  dprintf "Stub.talloc %Ld" size;
  match alloc t bisize () with
  | Result.Ok addr when not (addr %= bi0) ->
     (* The talloc counts as one operation, so pre-subtract
      * the increment of munmap. *)
     t.n_operations := !(t.n_operations) - 1;
     Some (munmap t ~addr ~size:bisize ())
  | _ ->
     (* n_operations increased by one in alloc *)
     None

let mmap_fixed t ?(shadow=false) ~addr ~size () =
  dprintf "Stub.mmap_fixed %#Lx %s" (int64_of_big_int addr) (humsize size);
  let vmas, ok = List.fold_left ~init:([], false) ~f:(fun (acc, ok) vma ->
    match vma with
    | `Allocated _ ->
       (vma :: acc, ok)
    | `Gap gap ->
       let fixed_high = addr %+ size in
       begin
         if gap.low %> addr then begin
           (* Not it *)
           (vma :: acc, ok)
         end else if gap.low %= addr then begin
           if gap.high %> fixed_high then begin
             (* We simply refuse to handle this case, we don't expect to need it *)
             failwith "mmap_fixed: TBD1"
           end else if gap.high %= fixed_high then begin
             (* Perfect fit, yay *)
             ((`Allocated {gap with flags = "----"; name = "[mmap_fixed]"}) :: acc, true)
           end else begin
             (* Area fits within the gap, can shrink the gap and add it below *)
             let vma = `Allocated {gap with
               high = fixed_high; flags = "----"; name ="[mmap_fixed]"}
             in
             let ngap = `Gap {gap with low = fixed_high} in
             (ngap :: vma :: acc, true)
           end
         end else begin
           (* gap.low < addr *)
           if gap.high %<= addr then begin
             (vma :: acc, ok)
           end else begin
             (* gap.low < addr && addr < gap.high i.e. addr falls within the gap *)
             if gap.high %< fixed_high then begin
               (* request extends past the gap; our caller must have messed up *)
               failwith "mmap_fixed: TBD2"
             end else if gap.high %= fixed_high then begin
             (* Area fits within the gap, can shrink the gap and add it above *)
               let ngap = `Gap {gap with high = addr} in
               let vma = `Allocated {gap with low = addr; flags = "----"; name = "[mmap_fixed]"} in
               (vma :: ngap :: acc, true)
             end else begin
               (* Area fits and will split the gap in two *)
               let gaplow = `Gap {gap with high = addr} in
               let vma = `Allocated {low = addr; high = fixed_high; flags = "----"; name = "[mmap_fixed]"} in
               let gaphigh = `Gap {gap with low = fixed_high} in
               (gaphigh :: vma :: gaplow :: acc, true)
             end
           end
         end
       end) !(t.vmas)
  in
  if not ok then begin
    failwith "mmap_fixed failed!"
  end;
  let vmas = List.rev vmas in
  t.vmas := vmas;
  t

let pid _ () =
  failwith "Called Shadow_stub.pid"

let refresh_interesting_holes stub () =
  (*
   * We adjust the stack to account for the guard page when parsing the
   * mapsstr; so when we generate a mapsstr to be parsed again, we need
   * to undo the adjustment.
   *)
  let vmas = !(stub.vmas) |> (adj_stack_guard_page StackAdjUp) in
  let mapsstr = do_maps vmas in
  let interesting_holes = do_get_interesting_holes mapsstr
    stub.interesting_holes_parser
  in
  {stub with interesting_holes}

let initial_size_for_hole stub name =
  match SM.find stub.interesting_holes name with
    | None -> failwith (sprintf "Couldn't find hole %s in map" name)
    | Some hole -> hole.high %- hole.low

let holes_of_interest stub = SM.data stub.interesting_holes

let lowest_gap stub () =
  match List.filter_map ~f:select_gaps !(stub.vmas) with
  | gap :: _ -> gap
  | [] -> failwith "Shadow_stub: No lowest gap!"
