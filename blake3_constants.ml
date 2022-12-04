(* The block size in bytes. *)
let block_size = 64

(* The number of rounds to run. *)
let rounds = 12

(* The initialization vector for the hash. *)
let iv =
  [ 0x6a09e667_f3bcc908L; 0xbb67ae85_84caa73bL;
    0x3c6ef372_fe94f82bL; 0xa54ff53a_5f1d36f1L;
    0x510e527f_ade682d1L; 0x9b05688c_2b3e6c1fL;
    0x1f83d9ab_fb41bd6bL; 0x5be0cd19_137e2179L; ]
    
let c =
  [ 0x243f6a8885a308d3L; 0x13198a2e03707344L;
    0xa4093822299f31d0L; 0x082efa98ec4e6c89L;
    0x452821e638d01377L; 0xbe5466cf34e90c6cL;
    0xc0ac29b7c97c50ddL; 0x3f84d5b5b5470917L; 
    0x9216d5d98979fb1bL; 0xd1310ba698dfb5acL;
    0x2ffd72dbd01adfb7L; 0xb8e1afed6a267e96L; 
    0xba7c9045f12c7f99L; 0x24a19947b3916cf7L; 
    0x0801f2e2858efc16L; 0x636920d871574e69L; ]
