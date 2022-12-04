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
