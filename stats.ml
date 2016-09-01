open Core.Std
open Sexplib_num.Std.Big_int
open Printf

module type ValueType =
sig
  type t
  val float_of_t : t -> float
  val float_to_s : float -> string
end

module BigIntValue =
struct
  type t = big_int
  let float_of_t = float_of_big_int
  let float_to_s f = f |> Float.to_int64 |> big_int_of_int64 |> Util.humsize
end

module IntValue =
struct
  type t = int
  let float_of_t = Float.of_int
  let float_to_s = sprintf "%.1f"
end

module Make(ValueType : ValueType) =
struct
  type t = {
    name : string;
    sum : float;
    count : int;
    min : float;
    max : float;
    values : float list;
  }
  let make name = {
    name;
    sum = 0.;
    count = 0;
    min = Float.max_finite_value;
    max = 0.;
    values = [];
  }

  let add_value t v =
    let v = ValueType.float_of_t v in
    {t with
      sum = t.sum +. v;
      count = t.count + 1;
      min = if v <. t.min then v else t.min;
      max = if v >. t.max then v else t.max;
      values = v :: t.values;
    }

  let to_s {name; sum; count; min; max; values} =
    let values = List.sort ~cmp:Float.compare values in
    let median l =
      let len = List.length l in
      match len % 2 with
      | 0 ->
         let low = List.nth_exn l (len / 2) in
         let high = List.nth_exn l ((len / 2) + 1) in
         (low +. high) /. 2.
      | 1 ->
         List.nth_exn l ((len / 2) + 1)
      | _ -> failwith "mod 2 not in {0, 1}"
    in
    let ppf = ValueType.float_to_s in
    sprintf "%s: avg = %s, min = %s, max = %s, median = %s"
      name (ppf (sum /. (Float.of_int count))) (ppf min) (ppf max)
      (ppf (median values))
  let str_values {values; _} =
    List.map ~f:(sprintf "%f") values
end

module BigIntStats = Make(BigIntValue)
module IntStats = Make(IntValue)
