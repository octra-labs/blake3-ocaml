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
