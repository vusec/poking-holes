open Core.Std
open Sexplib_num.Std.Big_int
open Big_int_util
open Util

let dprintf = Debug.eprintf

module Hoi = Hole_of_interest

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
module SM = Map.Make(String)

let vma_size (vma : vma) = vma.high %- vma.low

let vma_to_s (vma : vma) = sprintf "{vma (%s, %s, %s, '%s')}"
  (hex_of_bi vma.low) (hex_of_bi vma.high) vma.flags vma.name

let vma_to_mapsline vma =
  let (vma, flags) = match vma with
    | `Gap vma -> (vma, "gap")
    | `Allocated vma -> (vma, vma.flags)
  in
  sprintf "%20s%#20Lx %#20Lx\t%s\t%s" (fmtsz (vma_size vma))
    (int64_of_big_int vma.low) (int64_of_big_int vma.high) flags vma.name

let vmas_equal (vma1 : vma) (vma2 : vma) =
  vma1.low %= vma2.low && vma1.high %= vma2.high &&
  String.equal vma1.flags vma2.flags

let unbox_vma = function
  | `Allocated vma
  | `Gap vma -> vma

let vma_is_userland (vma : vma) = vma.high %<= biUserspaceUpperBound

type stack_adjustment =
  | StackAdjUp
  | StackAdjDown
(*
 * The kernel will not allocate the guard page of the stack
 * unless one uses MAP_FIXED, so might as well consider it
 * mapped. This matters even for large allocations because
 * if we happen to allocate 100MB and the size of the hole
 * is exactly 100MB (to the page), then the allocation will
 * fail. It's peanuts for finding the size of the hole, but
 * the ShadowStub should not allow this allocation.
 *)
let adj_stack_guard_page op vmas =
  let do_adj = match op with
    | StackAdjUp -> (%+)
    | StackAdjDown -> (%-)
  in
  let maybe_fixup ~curr ~prev =
    match curr with
    | `Allocated vma ->
       begin
         if String.equal "[stack]" vma.name then begin
           (* XXX: this assumes the stack grows downwards *)
           `Allocated {vma with low = do_adj vma.low pagesize}
         end else begin
           curr
         end
       end
    | `Gap vma ->
       begin
         match prev with
         | `Gap prev ->
            let (s : string) = sprintf "Gap %s followed by gap %s"
              (vma_to_s vma) (vma_to_s prev) in
            failwith s
         | `Allocated prev ->
           if String.equal "[stack]" prev.name then begin
              (* Stack has already been adjusted, see above *)
              let vma = {vma with high = do_adj vma.high pagesize} in
              assert (vma.high %= prev.low);
              `Gap vma
            end else begin
              curr
            end
       end
  in
  List.fold_right ~init:[] ~f:(fun curr acc ->
    match acc with
    | [] -> curr :: acc
    | prev :: _ -> (maybe_fixup ~curr ~prev) :: acc) vmas

let get_vmas mapsstr =
  let gapre = Str.regexp ".*gap.*" in
  let is_gap str = Str.string_match gapre str 0 in
  let re = Str.regexp "^[ \t]+[^ \t]+[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)" in
  let area_of_line line =
    if not (Str.string_match re line 0) then begin
      failwith (sprintf "Couldn't parse %s" line)
    end else begin
      let name = Str.matched_group 4 line in
      let flags = Str.matched_group 3 line in
      let low = Str.matched_group 1 line in
      let high = Str.matched_group 2 line in
      let low = big_int_of_string low in
      let high = big_int_of_string high in
      let low, high =
        if low %< biUserspaceUpperBound &&
          high %> biUserspaceUpperBound then begin
          (* Area C extends into kernel space, but the allocatable part
           * is only up to biUserspaceUpperBound *)
            (low, biUserspaceUpperBound)
          end else if low %= bi0 then begin
            if high %<= mmap_min && not (is_gap line) then begin
              let s = sprintf "VMA (%s, %s) is within the mmap_min limit"
                (hex_of_bi low) (hex_of_bi high) in
              failwith s
            end;
            (mmap_min, high)
          end else begin
            low, high
          end
      in
      let vma = {
        low;
        high;
        flags;
        name;
      } in
      if is_gap line then begin
        `Gap vma
      end else begin
        `Allocated vma
      end
    end
  in
  mapsstr |> String.split_lines |> List.map ~f:area_of_line |>
      List.filter ~f:(fun vma -> vma_is_userland (unbox_vma vma)) |>
          (adj_stack_guard_page StackAdjDown)

let vma_includes vma addr = vma.low %<= addr && vma.high %> addr

let select_allocated = function
  | `Allocated vma -> Some vma
  | `Gap _ -> None

let select_gaps = function
  | `Allocated _ -> None
  | `Gap vma -> Some vma

let get_gaps mapsstr =
  mapsstr |> get_vmas |> List.filter_map ~f:select_gaps

let h_of_vma name behavior vma =
  {
    Hoi.name = name;
    Hoi.behavior = behavior;
    Hoi.low = vma.low;
    Hoi.high = vma.high
  }

let get_interesting_holes_by_size hole_descrs mapsstr =
  let vmas = get_gaps mapsstr in
  let vmas = List.sort ~cmp:(fun vma1 vma2 ->
    compare_big_int (vma_size vma2) (vma_size vma1)) vmas in
  if List.length hole_descrs > List.length vmas then begin
    failwith "Not enough vmas for hole_descrs"
  end;
  let vmas = List.take vmas (List.length hole_descrs) in
  List.map2_exn ~f:(fun vma hdescr ->
    h_of_vma hdescr.Hole_descr.name hdescr.Hole_descr.behavior vma) vmas hole_descrs

let get_gap_before_stack vmas =
  let _, found = List.fold_left ~init:(None, None) ~f:(fun (prev, found) curr ->
    match (found : vma option) with
    | Some _ ->
       (None, found)
    | None ->
       begin
         match curr with
         | `Gap _ ->
            (Some curr, None)
         | `Allocated curr_vma ->
            begin
              if String.equal curr_vma.name "[stack]" then begin
                match prev with
                | Some (`Gap prev) ->
                   (None, Some prev)
                | Some (`Allocated _) ->
                   failwith "vma before stack is not a gap"
                | None ->
                   failwith "No gap before stack vma!"
              end else begin
                (Some curr, None)
              end
            end
       end) vmas
  in
  match found with
  | None -> failwith "failed to find gap before stack"
  | Some vma -> vma

let get_interesting_holes_by_pos hole_descrs_by_pos mapsstr =
  let vmas = get_vmas mapsstr in
  let gaps = List.filter_map ~f:select_gaps vmas in
  let gaps_ary = Array.of_list gaps in
  let open Monparnes_config in
  let gaps_for_pos hdescr_by_pos = match hdescr_by_pos with
    | AbsPosAndSortSize (base, len, hdescrs) ->
       if len <> List.length hdescrs then begin
         let str = Sexp.to_string_hum (Monparnes_config.sexp_of_hole_descr_by_pos hdescr_by_pos) in
         failwith (sprintf "Inconsistent lengths: %s" str)
       end;
       begin
         try
           let ary = Array.slice gaps_ary base (base + len) in
           Array.sort ary ~cmp:(fun g1 g2 -> compare_big_int (vma_size g2) (vma_size g1));
           List.zip_exn (Array.to_list ary) hdescrs
         with Invalid_argument _ ->
           let s = sprintf "Hole descriptions for pos %d, len %d but no such holes"
             base len in
           failwith s
       end
    | BeforeStack hdescr ->
       [(get_gap_before_stack vmas, hdescr)]
    | EndOfUserspace hdescr ->
       [(Array.last gaps_ary, hdescr)]
  in
  List.fold_left ~init:[] ~f:(fun hois hdesr_by_pos ->
    let gaps_and_hdesrs = gaps_for_pos hdesr_by_pos in
    List.fold_left ~init:hois ~f:(fun acc (gap, hdescr) ->
      let hoi = h_of_vma hdescr.Hole_descr.name hdescr.Hole_descr.behavior gap in
      if List.exists ~f:(fun hole -> hole.Hoi.low %= hoi.Hoi.low) hois then begin
        let hstr = Sexp.to_string_hum (Hole_descr.sexp_of_t hdescr) in
        let s = sprintf "Duplicate descr for hole at %#Lx (%s)"
          (int64_of_big_int hoi.Hoi.low) hstr in
        failwith s
      end;
      hoi :: acc
    ) gaps_and_hdesrs
  ) hole_descrs_by_pos

let hole_of_interest_for_gap holes_of_interest gap =
  let holes = List.filter ~f:(fun hole ->
    gap.low %>= hole.Hoi.low && gap.high %<= hole.Hoi.high) holes_of_interest in
  match holes with
    | [h] -> h
    | [] ->
      failwith (sprintf "No hole for gap %s" (vma_to_s gap))
    | _ -> (* OK, THIS we're not expecting. Ever. *)
      failwith (sprintf "Multiple holes for gap %s" (vma_to_s gap))

let vma_overlap vma ~low ~high =
  if vma.high %< low || vma.low %> high then begin
    bi0	(* No overlap *)
  end else if vma.low %< low then begin
    if vma.high %<= high then begin
      vma.high %- low (* partial overlap from below *)
    end else begin
      high %- low (* vma contains what used to be the gap *)
    end
  end else begin
    assert (low %<= vma.low && vma.low %<= high);
    if vma.high %> high then begin
      high %- vma.low (* partial overlap from above *)
    end else begin
      vma.high %- vma.low (* vma is contained in the gap *)
    end
  end

let determine_current_hole_size vmas hole =
  let low = hole.Hoi.low in
  let high = hole.Hoi.high in
  List.fold_left ~f:(fun acc vma ->
    let overlap = vma_overlap vma ~low ~high in
    acc %+ overlap
  ) ~init:bi0 vmas

let actual_hole_sizes interesting_holes mapsstr =
  let vmas = get_gaps mapsstr in
  let f hole =
    (hole.Hoi.name, determine_current_hole_size vmas hole) in
  let ret = List.map ~f (SM.data interesting_holes) in
  List.iter ~f:(fun (name, size) ->
    if not ((size %% pagesize) %= bi0) then
      failwith (sprintf "hole %s: not an integral number of pages (%s)" name
                  (string_of_big_int size)))
    ret;
  ret

let do_get_interesting_holes mapsstr gih =
  let interesting_holes = gih mapsstr in
  List.iter ~f:(fun hole ->
    if not (((hole.Hoi.high %- hole.Hoi.low) %% pagesize) %= bi0) then begin
      failwith (sprintf "%s: not an integral number of pages" (Hoi.to_s hole))
    end;
    dprintf "Hole_of_interest: %s" (Hoi.to_s hole))
    interesting_holes;
  let alist = List.map ~f:(fun hole -> (hole.Hole_of_interest.name, hole)) interesting_holes in
  match SM.of_alist alist with
  | `Duplicate_key key ->
     failwith (sprintf "Duplicate hole (%s) for interesting holes" key)
  | `Ok interesting_holes ->
     interesting_holes
