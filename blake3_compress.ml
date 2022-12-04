
open Blake3_constants
open Blake3_mix

(* The compression function for the hash. *)
let compress state message =
  let s = Array.make 16 0l in
  for i = 0 to 7 do
    s.(i) <- state.(i)
  done;
  for i = 0 to 7 do
    s.(i+8) <- iv.(i)
  done;
  for i = 0 to 15 do
    s.(i) <- s.(i) lxor (Int64.of_int message.(i))
  done;
