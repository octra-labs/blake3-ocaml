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

