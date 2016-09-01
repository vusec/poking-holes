open Sexplib_num.Std.Big_int

let (%-) = sub_big_int
let (%%) = mod_big_int
let (%/) = div_big_int
let (%+) = add_big_int
let ( %* ) = mult_big_int
let (%<=) = le_big_int
let (%<) = lt_big_int
let (%>) = gt_big_int
let (%>=) = ge_big_int
let (%=) = eq_big_int
let bi0 = zero_big_int
let bi1 = big_int_of_int 1
let bi2 = big_int_of_int 2
let bi3 = big_int_of_int 3
let bi4 = big_int_of_int 4
let bi5 = big_int_of_int 5
let bi6 = big_int_of_int 6
let bi8 = big_int_of_int 8
let bi10 = big_int_of_int 10
let bi1024 = big_int_of_int 1024
let bi1MiB = bi1024 %* bi1024
let bi1GiB = bi1MiB %* bi1024
let bi1TiB = bi1GiB %* bi1024
let bi1PiB = bi1TiB %* bi1024
let pagesize = big_int_of_int 4096 (* XXX: config.ml? *)

(* We cannot allocate the highest page of the user address space *)
(* Remember, high bounds are not inclusive, hence + 1 *)
let biUserspaceUpperBound = big_int_of_string "0x7fffffffffff" %- pagesize %+ bi1

(* XXX: we should really be asking the stub to check the sysctl limit for us *)
let mmap_min = big_int_of_string "65536"
