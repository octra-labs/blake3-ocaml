(* The mixing function for the compression function. *)
let g s a b c d e f g h =
  let a = a + b + s.(e) in
  let d = d lxor a in
  let d = d lsl 32 - 16 in
  let c = c + d in
  let b = b lxor c in
  let b = b lsr 24 in
  let a = a + b + s.(g) in
  let d = d lxor a in
  let d = d lsl 16 - 16 in
  let c = c + d in
  let b = b lxor c in
  let b = b lsr 16 in
  let a = a + b + s.(h) in
  let d = d lxor a in
  let d = d lsl 63 - 32 in
  let c = c + d in
  let b = b lxor c in
  let b = b lsr 63 in
  (a, b, c, d)



(* A Counter is a data structure that tracks the number of 0 and 1 bits in a given context. *)
type t = {
  mutable current_state: int;
}

type state = {
  count_zero: int;
  count_one: int;
  next_state_zero: int;
  next_state_one: int;
  next_state_zero_with_increment: int;
  next_state_one_with_increment: int;
  probability_zero: float;
  probability_one: float;
}

let state_table =
  [
  
  ]

let create () = {
  current_state = 0;
}

let get_zero c =
  state_table.(c.current_state).count_zero

let get_one c =
  state_table.(c.current_state).count_one

let priority c =
  get_zero c + get_one c

let increment c y =
  if y then
    if c.current_state < 208 || Random.float 1.0 < state_table.(c.current_state).probability_one then
      c.current_state <- state_table.(c.current_state).next_state_one_with_increment
    else
      c.current_state <- state_table.(c.current_state).next_state_one

let mix_vectors v1 v2 =
  let n = Array.length v1 in
  let result = Array.make n 0 in
  for i = 0 to n - 1 do
    result.(i) <- if v1.(i) = v2.(i) then v1.(i) else 1
  done;
  result
;;
