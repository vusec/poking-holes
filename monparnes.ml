open Core.Std
open Sexplib_num.Std.Big_int
open Printf
open Big_int_util
open Util

let dprintf = Debug.eprintf


let config = ref (Monparnes_config.of_params ())

let opt_shadow = ref true

(*
 * Generate the (k, n) combinations (i.e. n choose k) but also
 * supply the remaining elements after each pick. So for [1; 2; 3; 4; 5]
 * and k = 2 we get [f [1; 2] [3; 4; 5]; f [1; 3] [2; 4; 5]; ...]
 *)
let combinations_with_remainder ~f count lst =
  let rec select ~selected ~prev nrem ~rest ~acc =
    if nrem = 0 then begin
      (f (List.rev selected) ((List.rev prev) @ rest)) :: acc
    end else begin
      match rest with
	| x :: rest ->
	  (* Choose x *)
	  let acc = select ~selected:(x :: selected) ~prev (nrem - 1) ~rest ~acc in
	  (* Do not choose x *)
	  select ~selected ~prev:(x :: prev) nrem ~rest ~acc
	| [] ->
	  acc
    end
  in
  List.rev (select ~selected:[] ~prev:[] count ~rest:lst ~acc:[])

module type Stub =
sig
  type t
  val alloc : t -> ?shadow:bool -> big_int -> unit -> (big_int, Error.t) Result.t
  val maps : t -> unit -> string
  val initial_size_for_hole : t -> string -> big_int
  val holes_of_interest : t -> Hole_of_interest.t list
  val n_operations : t -> int
end


module Strategy(Stub : Stub) =
struct

  (* Split the interval between high and low according to config.split
   * and return the allocation sizes in reverse order.
   *)
  let steps ~high ~low acc =
    let size = high %- low in
    let step = size %/ (big_int_of_int (!config).split) in
    let rec inner n acc =
      if n < (!config).split then begin
        let sz = high %- ((big_int_of_int n) %* step) in
        let rem = sz %% pagesize in
        (* Bring up to next page *)
        let sz =
          if rem %> bi0 then begin
            sz %- rem %+ pagesize
          end else begin
            sz
          end
        in
        if sz %<= low then begin
          failwith (sprintf "sz %s <= low %s" (humsize sz) (humsize low));
        end;
        (* Remove runs of identical elements; this always happens between
         * calls to this function (as our last, lowest, value is fed back to
         * us as the new "high" value) and when size is so small that we
         * end up rounding elements to the same page number.
         *)
        match acc with
        | [] ->
           inner (n + 1) (sz :: acc)
        | prev :: _ ->
           if prev %= sz then begin
             inner (n + 1) acc
           end else begin
             inner (n + 1) (sz :: acc)
           end
      end else begin
        match acc with
        | [] -> failwith "Empty acc what?"
        | x :: _ ->
           (x, acc)
      end
    in
    inner 0 acc

  let rec do_calculate_steps ~high ~low acc =
    let diff = high %- low in
    if diff %<= bi0 then begin
      failwith (sprintf "high %s <= low %s"
		  (humsize high) (humsize low))
    end else if diff %= pagesize then begin
      (* Corner case; non-recursive call with high = low + pagesize; we
       * need to try and allocate high in order to make progress.
       *)
      match acc with
      | [] -> [high]
      | _ -> List.rev acc
    end else begin
      let (high, acc) = steps ~high ~low acc in
      do_calculate_steps ~high ~low acc
    end

  (* Given a set of states and the second largest maximum possible size of
   * a hole (max2), calculate the step we will use to descend to max 2.
   * Return a list of the next allocations we'll try or the empty list
   * if we can't descend any more.
   *)
  let calculate_steps ~max1 ~max2 =
    let sizes = do_calculate_steps ~high:max1 ~low:max2 [] in
(*    dprintf "Calculated sizes(%s -> %s): %s" (humsize max1) (humsize max2)
      (String.concat ~sep:", " (List.map ~f:humsize sizes));*)
    sizes

  (* We know an allocation just came from this hole *)
  let alloc_from_hole size hole =
    let hole = Hole.set_max hole (hole.Hole.max %- size) in
    let hole = Hole.set_total hole (hole.Hole.total %+ size) in
    hole

  (*
   * We know an allocation of this size just failed, so the hole
   * can't be larger than that.
   *)
  let hole_smaller_than size hole =
    (* XXX: don't go below zero (won't be a problem in practice *)
    if hole.Hole.max %<= (size %- pagesize) then begin
      hole
    end else begin
      let hole = Hole.set_max hole (size %- pagesize) in
      assert (hole.Hole.max %> bi0);
      hole
    end

  let state_update_exactly_count_succeeded ?expected_candidates size ~count state =
    (* Holes which we might have been able to allocate size bytes from *)
    let candidates, definitely_not_allocated_from =
      List.partition_tf ~f:(fun h -> h.Hole.max %>= size) state.States.holes in
    begin
      (* Perhaps our caller has specific expectations *)
      match expected_candidates with
	| Some n when n <> (List.length candidates) ->
	  let cstrs = List.map ~f:Hole.to_s candidates in
	  failwith (sprintf "candidates (%s) <> expected_candidates (%d)"
		      (String.concat ~sep:", " cstrs) n)
	| _ -> ()
    end;
    (*
     * Size was allocated count times and we don't know which holes
     * the allocation came from.
     *)
    let f allocated_from not_allocated_from =
      let els1 = List.map ~f:(alloc_from_hole size) allocated_from in
      let els2 = List.map ~f:(hole_smaller_than size) not_allocated_from in
      let nholes = List.append (List.append els1 els2) definitely_not_allocated_from in
      let nholes = List.sort ~cmp:Hole.namecmp nholes in
      {States.holes = nholes}
    in
    let ncandholes = List.length candidates in
    if ncandholes < count then begin
      []	(* impossible state *)
    end else begin
      combinations_with_remainder ~f count candidates
    end

  let multiplicity_sizes ~bottom init_size =
    let rec inner acc size =
      let size = (size %* bi5) %/ bi6 in
      let rem = size %% pagesize in
      (* Round up to the nearest multiple of the pagesize *)
      let size = size %- rem %+ pagesize in
      if size %<= bottom then begin
        acc
      end else begin
        inner (size :: acc) size
      end
    in
    let ret = List.rev (inner [] init_size) in
    dprintf "multiplicity_sizes: %s" (String.concat ~sep:", "
                                        (List.map ~f:humsize ret));
    ret

  (* Try ~count allocs *)
  let try_allocs stub ~size ~count =
    let rec inner ~n ~acc =
      match n with
	| 0 ->
         acc
	| _ ->
          begin
            match Stub.alloc ~shadow:(!opt_shadow) stub size () with
              | Result.Ok addr when addr %> bi0 ->
		dprintf "=> Stub.alloc %s -> OK" (humsize size);
		inner ~n:(n -1) ~acc:(acc + 1)
              | _ ->
		dprintf "=> Stub.alloc %s -> fail" (humsize size);
		acc
          end
    in
    inner ~n:count ~acc:0

  type maxgroup = {
    maxval : big_int;
    highest_multiplicity : int;
  }
  let maxgroup_to_s {maxval; highest_multiplicity} =
    sprintf "max: %s, multiplicity: %d" (string_of_big_int maxval) highest_multiplicity

  let determine_maxgroups maxes =
    let open States in
    let mhs_cmp a b = compare_big_int b.value a.value in
    let sorted = List.sort ~cmp:mhs_cmp maxes in
    let groups = List.group ~break:(fun a b -> not (a.value %= b.value)) sorted in
    List.map ~f:(fun grp ->
      (* grp is a list of maximum_hole_sizes that all have the same value *)
      let cmp a b = b.multiplicity - a.multiplicity in
      match List.max_elt ~cmp grp with
      | Some {value; multiplicity} ->
         {maxval = value; highest_multiplicity = multiplicity}
      | None ->
         failwith "empty maxvalue group"
    ) groups

  (*
   * Look into the live states and decide on our next move.
   *)
  let rec redispatch stub states : States.t =
    ignore(match States.check_sizes states (Stub.holes_of_interest stub) with
    | Ok _ ->
       ()
    | Error err ->
       begin
         dprintf "Erroneous states: %s" (States.to_s states);
         dprintf "%s" (Error.to_string_hum err);
         failwith "Inconsistency detected"
       end);
    dprintf "redispatch %s\n%!" (States.to_s states);
    let maxgroups = determine_maxgroups (States.max_hole_sizes states) in
    match maxgroups with
      | [] ->
	begin
	  failwith "empty groups"
	end
      | [{maxval; _};] when maxval %< (!config).mshs ->
	dprintf "Done! maxval %s < %s" (humsize maxval) (humsize (!config).mshs);
	states
      | [{maxval; highest_multiplicity}] ->
	begin
	  let max1 = maxval in
          (*          let sizes = multiplicity_sizes ~bottom:(!config).mshs maxval in*)
          let max2 = (!config).mshs %+ pagesize in
	  (* Changing max2 invalidates our grouping. See if it now
	   * happens to be equal to max1 *)
	  if max2 %= max1 then begin
		(* Eh, we could go to do_equal but then it'd have to know about
		 * our adjustment of max2 and who cares about a page anyway *)
	    states
	  end else begin
            let sizes = calculate_steps ~max1 ~max2 in
	    descend stub states ~max2 ~multiplicity:highest_multiplicity sizes
          end
	end
      | m1 :: (m2 :: _) ->
	begin
	  let max1 = m1.maxval in
	  let max2 = m2.maxval in
          (* It's the multiplicity of the highest maximum value that counts *)
          let multiplicity = m1.highest_multiplicity in
	  if max1 %<= (!config).mshs then begin
	    dprintf "Done! (maxval1 %s) <= %s" (humsize m1.maxval)
              (humsize (!config).mshs);
	    states
	  end else begin (* max1 is over the bound (i.e. OK) *)
	    (* What about max2 ? *)
	    if max2 %<= (!config).mshs then begin
	      (* max2 is below the bound. We can /still/ try and descend from
	       * max1, but we have to stop before trying to allocate maxbound!
	       * So pretend that max2 is just above the bound. NOTE: this
	       * means we should never re-fetch max2 in any of the callees!
	       *)
	      let max2 = (!config).mshs %+ pagesize in
	      (* Changing max2 invalidates our grouping. See if it now
	       * happens to be equal to max1 *)
	      if max2 %= max1 then begin
		(* Eh, we could go to do_equal but then it'd have to know about
		 * our adjustment of max2 and who cares about a page anyway *)
		states
	      end else begin
                let sizes = calculate_steps ~max1 ~max2 in
                descend stub states ~max2 ~multiplicity sizes
	      end
	    end else begin
              let sizes = calculate_steps ~max1 ~max2 in
              descend stub states ~max2 ~multiplicity sizes
	    end
	  end
	end
  and descend stub states ~max2 ~multiplicity sizes =
    dprintf "%s %s" (color ~color:`Green "Descent")
      (color ~color:`Yellow (sprintf "[multiplicity = %d]" multiplicity));
    let check_size size =
      if not ((size %% pagesize) %= bi0) then begin
        failwith (sprintf "size %s not a multiple of the page size %s)"
                    (humsize size) (humsize pagesize))
      end;
      let diff = compare_big_int size max2 in
      if diff = 0 then begin
        failwith (sprintf "size %s equal to max2 %s"
		    (humsize size) (humsize max2))
      end else if diff < 0 then begin
        failwith (sprintf "size %s went below max2 %s"
		    (humsize size) (humsize max2))
      end
    in
    match sizes with
    | [] ->
       (* We got as close to max2 as we could, can't descend any more *)
       redispatch stub states
    | size :: sizes ->
       begin
         dprintf "descend:%s %s"
           (humsize size) (States.to_s states);

         check_size size;
         let allocs_succeeded = try_allocs stub ~size ~count:multiplicity in
         if allocs_succeeded > 0 then begin
           let f = state_update_exactly_count_succeeded size
             ~count:allocs_succeeded in
	   let states = States.multimap f states in
	   (* Some allocation took place, need to re-examine the situation *)
	   redispatch stub states
         end else begin
	   (* No allocation succeeded, recurse *)
	   (* Update the max sizes *)
           let f = state_update_exactly_count_succeeded size ~count:0 in
	   let states = States.multimap f states in
           descend stub states ~max2 ~multiplicity sizes
      end
    end

  let deduce start_time stub ~save_results_to_path ~save_maps_to_path init_state =
    let initial_maps = Stub.maps stub () in
    ignore(match save_maps_to_path with
    | None -> ()
    | Some path -> Out_channel.write_all path ~data:initial_maps);
    printf "%s%!" initial_maps;
    let states = States.create [init_state] in
    let states = redispatch stub states in
    let end_time = Time.now () in
    let res = {
      Results.states = states;
      Results.n_operations = Stub.n_operations stub;
      Results.interesting_holes = Stub.holes_of_interest stub;
      Results.runtime = Time.diff end_time start_time;
    } in
    printf "Final states: %s\n" (States.to_s states);
    printf "%s%!" (Stub.maps stub ());
    Results.summarize res;
    begin
      match save_results_to_path with
      | None -> ()
      | Some path -> Sexp.save_hum path (Results.sexp_of_result res)
    end;
end

let do_example ex =
  match ex with
  | Some "simple" ->
     Sexp.output_hum stdout (Sexplib.Conv.sexp_of_list
                               Monparnes_config.sexp_of_hole_descr_by_pos
                               Monparnes_config.simple_holes);
    printf "\n";
    exit 0
  | Some "hidden_obj" ->
     Sexp.output_hum stdout (Sexplib.Conv.sexp_of_list
                               Monparnes_config.sexp_of_hole_descr_by_pos
                               Monparnes_config.hidden_object_holes);
    printf "\n";
    exit 0
  | Some other ->
     eprintf "Unknown example: '%s'\n" other;
    exit 2
  | None ->
     ()

let init_state hole_descrs_by_pos =
  let open Monparnes_config in
  let hdescrs = List.map ~f:(function
    | AbsPosAndSortSize (_, _, hdescrs) -> hdescrs
    | BeforeStack hdescr -> [hdescr]
    | EndOfUserspace hdescr -> [hdescr]) hole_descrs_by_pos
  in
  let hdescrs = List.concat hdescrs in
  States.state_of_hole_descrs hdescrs

let doit load_from save_results_to_path save_maps_to_path live_cmd configpath
    example disable_shadow_stub () =
    do_example example;
    Random.self_init ();
    let start_time = Time.now () in
    begin
      match configpath with
      | Some path -> config := Monparnes_config.t_of_sexp (Sexp.load_sexp path)
      | None -> ()
    end;
    dprintf "Using config:\n%s" (Sexp.to_string_hum (Monparnes_config.sexp_of_t !config));
    let init_state = init_state (!config).hole_descrs_by_pos in
    opt_shadow := not disable_shadow_stub;
    match load_from with
      | None ->
         let live_cmd = Option.value ~default:"./stub-pie" live_cmd in
         let gih = Live_stub.get_interesting_holes_by_pos (!config).hole_descrs_by_pos in
         let stub = Live_stub.create gih live_cmd in

         let module StubUtil = Stub_util.Make(Live_stub) in
         let stub = StubUtil.perform_preallocations stub ~shadow:(!opt_shadow) (!config).preallocations in

         let module Strategy = Strategy(Live_stub) in
         Strategy.deduce start_time stub ~save_results_to_path ~save_maps_to_path init_state

      | Some path ->
         if not !opt_shadow then begin
           failwith "Meaningless option to disable the shadow stub on a shadow run"
         end;
         let mapsstr = In_channel.read_all path in
         let gih = Shadow_stub.get_interesting_holes_by_pos (!config).hole_descrs_by_pos in
         let stub = Shadow_stub.create gih mapsstr in

         let module StubUtil = Stub_util.Make(Shadow_stub) in
         let stub = StubUtil.perform_preallocations stub (!config).preallocations in

         let module Strategy = Strategy(Shadow_stub) in
         Strategy.deduce start_time stub ~save_results_to_path ~save_maps_to_path init_state

let command =
  Command.basic
    ~summary:"Deduce the potential size of holeA"
    Command.Spec.(
      empty
      +> flag "-l" (optional string) ~doc:"PATH Load initial memory configuration from path"
      +> flag "-r" (optional string) ~doc:"PATH Save results to path"
      +> flag "-s" (optional string) ~doc:"PATH Save initial memory configuration in path"
      +> flag "-c" (optional string) ~doc:"STRING Command to use as stub"
      +> flag "-C" (optional string) ~doc:"PATH Load algorithm parameters from path"
      +> flag "-E" (optional string) ~doc:"EXAMPLE Dump example for [simple|hidden_obj]"
      +> flag "-noshadow" (no_arg) ~doc:"Disable shadow stub"
    ) doit

let () =
  dprintf "Starting up";
  Command.run ~version:"0.1" command
