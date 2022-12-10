open Blake3_constants
open Blake3_mix

let compress state message =
  let s = Array.make 16 0L in
  for i = 0 to 7 do
    s.(i) <- state.(i)
  done;
  for i = 0 to 7 do
    s.(i+8) <- iv.(i)
  done;
  for i = 0 to 15 do
    s.(i) <- s.(i) lxor (Int64.of_int message.(i))
  done;
  let a, b, c, d = g s 0 1 2 3 4 5 6 7 in
  let a, b, c, d = g s a b c d 0 9 10 11 in
  let a, b, c, d = g s a b c d 4 13 14 15 in
  let a, b, c, d = g s a b c d 8 1 2 3 in
  let a, b, c, d = g s a b c d 12 5 6 7 in
  let a, b, c, d = g s a b c d 0 13 14 15 in
  let a, b, c, d = g s a b c d 4 9 10 11 in
  let a, b, c, d = g s a b c d 8 5 6 7 in
  let a, b, c, d = g s a b c d 12 1 2 3 in
  let a, b, c, d = g s a b c d 0 13 14 15 in
  let a, b, c, d = g s a b c d 4 9 10 11 in
  let a, b, c, d = g s a b c d 8 5 6 7 in
  let a, b, c, d = g s a b c d 12 1 2 3 in
  let a, b, c, d = g s a b c d 0 13 14 15 in
  let a, b, c, d = g s a b c d 4 9 10 11 in
  let a, b, c, d = g s a b c d 8 5 6 7 in
  let a, b, c, d = g s a b c d 12 1 2 3 in
  let a, b, c, d = g s a b c d 0 9 10 11 in
  let a, b, c, d = g s a b c d 4 5 6 7 in
  for i = 0 to 7 do
    state.(i) <- state.(i) lxor s.(i) lxor s.(i+8)
  done
  
  let gso (v : float array) : float array =
  let n = Array.length v in
  let u = Array.copy v in
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      let dot_product = ref 0.0 in
      for k = 0 to n - 1 do
        dot_product := !dot_product +. u.(k) *. v.(k)
      done;
      let scale_factor = !dot_product /. (u.(i) *. u.(i)) in
      for k = 0 to n - 1 do
        v.(k) := v.(k) -. scale_factor *. u.(k)
      done
    done;
    let norm = ref 0.0 in
    for k = 0 to n - 1 do
      norm := !norm +. v.(k) *. v.(k)
    done;
    let scale_factor = sqrt(!norm) in
    for k = 0 to n - 1 do
      u.(k) := v.(k) /. scale_factor
    done
  done;
  u

