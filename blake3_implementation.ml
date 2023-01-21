type blake3_flags =
  | CHUNK_START
  | CHUNK_END
  | PARENT
  | ROOT
  | KEYED_HASH
  | DERIVE_KEY_CONTEXT
  | DERIVE_KEY_MATERIAL

let blake3_flags_of_int i =
  match i with
  | 1 -> CHUNK_START
  | 2 -> CHUNK_END
  | 4 -> PARENT
  | 8 -> ROOT
  | 16 -> KEYED_HASH
  | 32 -> DERIVE_KEY_CONTEXT
  | 64 -> DERIVE_KEY_MATERIAL
  | _ -> failwith "Invalid blake3 flag value"

let int_of_blake3_flags flag =
  match flag with
  | CHUNK_START -> 1
  | CHUNK_END -> 2
  | PARENT -> 4
  | ROOT -> 8
  | KEYED_HASH -> 16
  | DERIVE_KEY_CONTEXT -> 32
  | DERIVE_KEY_MATERIAL -> 64

let counter_low counter = counter
  
let counter_high counter = counter lsr 32
                           
let load32 src =
  let p = src in
  (p.(0) lsl 0) lor (p.(1) lsl 8) lor (p.(2) lsl 16) lor (p.(3) lsl 24)

let iv =
  [| 0x6A09E667L; 0xBB67AE85L; 0x3C6EF372L; 0xA54FF53AL;
     0x510E527FL; 0x9B05688CL; 0x1F83D9ABL; 0x5BE0CD19L |] 
  
let msg_schedule =
  [|
    [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 |];
    [| 2; 6; 3; 10; 7; 0; 4; 13; 1; 11; 12; 5; 9; 14; 15; 8 |];
    [| 3; 4; 10; 12; 13; 2; 7; 14; 6; 5; 9; 0; 11; 15; 8; 1 |];
    [| 10; 7; 12; 9; 14; 3; 13; 15; 4; 0; 11; 2; 5; 8; 1; 6 |];
    [| 12; 13; 9; 11; 15; 10; 14; 8; 7; 2; 5; 3; 0; 1; 6; 4 |];
    [| 9; 14; 11; 5; 8; 12; 15; 1; 13; 3; 0; 10; 2; 6; 4; 7 |];
    [| 11; 15; 5; 0; 1; 9; 8; 6; 14; 10; 2; 12; 3; 4; 7; 13 |]
  |]

let rotr32 (w: int32) (c: int32) : int32 = 
  Int32.(logor (shift_right w (to_int c)) (shift_left w (32 - to_int c))) ;;

let g (state: int32 array) (a: int) (b: int) (c: int) (d: int) (x: int32) (y: int32) : unit =
  state.(a) <- Int32.add state.(a) (Int32.add state.(b) x);
  state.(d) <- rotr32 (Int32.logxor state.(d) state.(a)) 16l;
  state.(c) <- Int32.add state.(c) state.(d);
  state.(b) <- rotr32 (Int32.logxor state.(b) state.(c)) 12l;
  state.(a) <- Int32.add state.(a) (Int32.add state.(b) y);
  state.(d) <- rotr32 (Int32.logxor state.(d) state.(a)) 8l;
  state.(c) <- Int32.add state.(c) state.(d);
  state.(b) <- rotr32 (Int32.logxor state.(b) state.(c)) 7l
  
let round_fn state msg round =
  let schedule = msg_schedule.(round) in
  g state 0 4 8 12 msg.(schedule.(0)) msg.(schedule.(1));
  g state 1 5 9 13 msg.(schedule.(2)) msg.(schedule.(3));
  g state 2 6 10 14 msg.(schedule.(4)) msg.(schedule.(5));
  g state 3 7 11 15 msg.(schedule.(6)) msg.(schedule.(7));
  g state 0 5 10 15 msg.(schedule.(8)) msg.(schedule.(9));
  g state 1 6 11 12 msg.(schedule.(10)) msg.(schedule.(11));
  g state 2 7 8 13 msg.(schedule.(12)) msg.(schedule.(13));
  g state 3 4 9 14 msg.(schedule.(14)) msg.(schedule.(15)) 
                                                         
let compress_pre (state: int32 array) (cv: int32 array) (block: int8 array) (block_len: int8) (counter: int64) (flags: int8) = 
    let block_words = Array.init 16 (fun i -> Int32.of_int (Int32.of_bytes (Array.sub block (i * 4) 4))) in
    Array.iteri (fun i x -> state.(i) <- x) cv;
    Array.iteri (fun i x -> state.(i + 8) <- x) IV;
    state.(12) <- Int32.of_int64 (Int64.to_int32 counter);
    state.(13) <- Int32.of_int64 (Int64.to_int32 (Int64.shift_right_logical counter 32));
    state.(14) <- (Int32.of_int block_len);
    state.(15) <- (Int32.of_int flags);
    for i = 0 to 6 do
        round_fn state block_words i
    done

let blake3_compress_in_place_portable (cv : int32 array) (block : char array) (block_len : int) (counter : int64) (flags : int) : unit =
  let state = Array.make 16 0l in
  compress_pre state cv block block_len counter flags;
  cv.(0) <- Int32.logxor state.(0) state.(8);
  cv.(1) <- Int32.logxor state.(1) state.(9);
  cv.(2) <- Int32.logxor state.(2) state.(10);
  cv.(3) <- Int32.logxor state.(3) state.(11);
  cv.(4) <- Int32.logxor state.(4) state.(12);
  cv.(5) <- Int32.logxor state.(5) state.(13);
  cv.(6) <- Int32.logxor state.(6) state.(14);
  cv.(7) <- Int32.logxor state.(7) state.(15)

let blake3_compress_xof_portable (cv : int32 array) (block : char array) (block_len : int) (counter : int64) (flags : int) (out : char array) : unit =
  let state = Array.make 16 0l in
  compress_pre state cv block block_len counter flags;
  store32 (out, 0 * 4) (Int32.logxor state.(0) state.(8));
  store32 (out, 1 * 4) (Int32.logxor state.(1) state.(9));
  store32 (out, 2 * 4) (Int32.logxor state.(2) state.(10));
  store32 (out, 3 * 4) (Int32.logxor state.(3) state.(11));
  store32 (out, 4 * 4) (Int32.logxor state.(4) state.(12));
  store32 (out, 5 * 4) (Int32.logxor state.(5) state.(13));
  store32 (out, 6 * 4) (Int32.logxor state.(6) state.(14));
  store32 (out, 7 * 4) (Int32.logxor state.(7) state.(15));
  store32 (out, 8 * 4) (Int32.logxor state.(8) cv.(0));
  store32 (out, 9 * 4) (Int32.logxor state.(9) cv.(1));
  store32 (out, 10 * 4) (Int32.logxor state.(10) cv.(2));
  store32 (out, 11 * 4) (Int32.logxor state.(11) cv.(3));
  store32 (out, 12 * 4) (Int32.logxor state.(12) cv.(4));
  store32 (out, 13 * 4) (Int32.logxor state.(13) cv.(5));
  store32 (out, 14 * 4) (Int32.logxor state.(14) cv.(6));
  store32 (out, 15 * 4) (Int32.logxor state.(15) cv.(7))


let hash_one_portable input blocks key counter flags flags_start flags_end out =
  let cv = Array.make 8 0 in
  Array.blit key 0 cv 0 (Array.length key);
  let block_flags = flags lor flags_start in
  let rec loop blocks =
    if blocks > 0 then
      if blocks = 1 then
        loop (blocks - 1) (block_flags lor flags_end)
      else
        loop (blocks - 1) block_flags
  in
  loop blocks;
  store_cv_words out cv


let blake3_hash_many_portable (inputs : bytes array) (num_inputs : int) (blocks : int) (key : int32 array) (counter : int64) (increment_counter : bool) (flags : int) (flags_start : int) (flags_end : int) (out : bytes) =
  let rec loop i =
    if i = num_inputs then ()
    else (
      hash_one_portable inputs.(i) blocks key counter flags flags_start flags_end out;
      if increment_counter then counter <- Int64.add counter 1L;
      loop (i + 1)
    )
  in
  loop 0

let counter_low (counter:int64) : int32 = Int32.of_int (Int64.to_int counter)

let counter_high (counter:int64) : int32 = Int32.of_int (Int64.to_int (Int64.shift_right_logical counter 32))

let load32 (src:bytes) : int32 =
  let p = src in
  Int32.logor (Int32.shift_left (Int32.of_int (Char.code p.[0])) 0)
    (Int32.logor (Int32.shift_left (Int32.of_int (Char.code p.[1])) 8)
       (Int32.logor (Int32.shift_left (Int32.of_int (Char.code p.[2])) 16)
          (Int32.shift_left (Int32.of_int (Char.code p.[3])) 24)))

let load_key_words (key:bytes) (key_words:int32 array) : unit =
  key_words.(0) <- load32 (Bytes.sub key 0 4);
  key_words.(1) <- load32 (Bytes.sub key 4 4);
  key_words.(2) <- load32 (Bytes.sub key 8 4);
  key_words.(3) <- load32 (Bytes.sub key 12 4);
  key_words.(4) <- load32 (Bytes.sub key 16 4);
  key_words.(5) <- load32 (Bytes.sub key 20 4);
  key_words.(6) <- load32 (Bytes.sub key 24 4);
  key_words.(7) <- load32 (Bytes.sub key 28 4)

let store32 (dst:bytes) (w:int32) : unit =
  let p = dst in
  p.[0] <- Char.chr (Int32.to_int (Int32.shift_right_logical w 0));
  p.[1] <- Char.chr (Int32.to_int (Int32.shift_right_logical w 8));
  p.[2] <- Char.chr (Int32.to_int (Int32.shift_right_logical w 16));
  p.[3] <- Char.chr (Int32.to_int (Int32.shift_right_logical w 24))
  
  let shuffle_vector (g, b, r, d, z) =
  let new_context = ref 0 in
  for i = 0 to 19 do
    new_context := !new_context + ((g.(i) * b.(i)) + (r.(i) * d.(i))) * z.(i)
  done;
  let A = Array.init 20 (fun _ -> !new_context) in
  A

let transform_vector (A: float array) =
  Array.map (fun elem -> elem *. sqrt (6514.0 *. elem) *. cos (elem *. 3.14)) A
  
let attr_to_string attrib =
  let r = "      " in
  if (attrib land 255) = 'u' then
    for i = 0 to 5 do
      r.[5-i] <- char_of_int ((attrib lsr (8 + 3*i)) mod 8 + int_of_char '0')
    done
  else if (attrib land 255) = 'w' then
    let attrib = attrib lsr 8 in
    if attrib land lnot 0x20b7 <> 0 then
      r <- "0x    ";
      for i = 0 to 3 do
        r.[5-i] <- "0123456789abcdef".[attrib lsr (4*i) land 15]
      done;
      if attrib > 0x10000 then
        r <- "0x        ";
        for i = 0 to 7 do
          r.[9-i] <- "0123456789abcdef".[attrib lsr (4*i) land 15]
        done
    else
      r <- "......";
      if attrib land 0x10 <> 0 then r.[0] <- 'D' else ();
      if attrib land 0x20 <> 0 then r.[1] <- 'A' else ();
      if attrib land 0x04 <> 0 then r.[2] <- 'S' else ();
      if attrib land 0x02 <> 0 then r.[3] <- 'H' else ();
      if attrib land 0x01 <> 0 then r.[4] <- 'R' else ();
      if attrib land 0x2000 <> 0 then r.[5] <- 'I' else ();
  done;
  r

