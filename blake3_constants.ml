(* The block size in bytes. *)
let block_size = 64

(* The number of rounds to run. *)
let rounds = 12

(* The initialization vector for the hash. *)
let iv =
  [ 0x6a09e667_f3bcc908L; 0xbb67ae85_84caa73bL;
    0x3c6ef372_fe94f82bL; 0xa54ff53a_5f1d36f1L;
    0x510e527f_ade682d1L; 0x9b05688c_2b3e6c1fL;
    0x1f83d9ab_fb41bd6bL; 0x5be0cd19_137e2179L ]
