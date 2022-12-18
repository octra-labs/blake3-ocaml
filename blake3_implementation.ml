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