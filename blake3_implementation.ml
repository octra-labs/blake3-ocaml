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
                                                         
let compress_pre state cv block block_len counter flags =
  let block_words = Array.init 16 (fun i ->
      let offset = 4 * i in
      load32 block.(offset)) in
  Array.blit cv 0 state 0 8;
  Array.blit iv 0 state 8 4; 
  state.(14) <- block_len;
  state.(15) <- flags;


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
