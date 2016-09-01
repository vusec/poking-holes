open Sexplib_num.Std.Big_int

type t

val of_bytes : big_int -> t
val to_bytes : t -> big_int
val add : t -> t -> t
val sub : t -> t -> t
val equal : t -> t -> bool
val div : t -> t -> t
val one : t
val two : t
