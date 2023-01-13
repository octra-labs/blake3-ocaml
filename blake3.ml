let blake3_key_len = 32
let blake3_out_len = 32
let blake3_block_len = 64
let blake3_chunk_len = 1024
let blake3_max_depth = 54
let chunk_start = 1 lsl 0
let chunk_end = 1 lsl 1
let parent = 1 lsl 2
let root = 1 lsl 3
let keyed_hash = 1 lsl 4
let derive_key_context = 1 lsl 5
let derive_key_material = 1 lsl 6

type blake3_chunk_state = {
  cv: int32 array;
  mutable chunk_counter: int64;
  mutable buf: bytes;
  mutable buf_len: int;
  mutable blocks_compressed: int;
  mutable flags: int;
}

let chunk_state_init (self: blake3_chunk_state) (key: int32 array) (flags: int) =
  Array.blit key 0 self.cv 0 blake3_key_len;
  self.chunk_counter <- 0L;
  self.buf <- Bytes.make blake3_block_len '\x00';
  self.buf_len <- 0;
  self.blocks_compressed <- 0;
  self.flags <- flags

let chunk_state_reset (self: blake3_chunk_state) (key: int32 array) (chunk_counter: int64) =
  Array.blit key 0 self.cv 0 blake3_key_len;
  self.chunk_counter <- chunk_counter;
  self.blocks_compressed <- 0;
  self.buf <- Bytes.make blake3_block_len '\x00';
  self.buf_len <- 0

let chunk_state_len (self: blake3_chunk_state) : int =
  (blake3_block_len * self.blocks_compressed) + self.buf_len 

let chunk_state_fill_buf (self: blake3_chunk_state) (input: bytes) (input_len: int) : int =
  let take = blake3_block_len - self.buf_len in
  let take = if take > input_len then input_len else take in
  let dest = Bytes.sub self.buf self.buf_len take in
  Bytes.blit input 0 dest 0 take;
  self.buf_len <- self.buf_len + take;
  take

let chunk_state_maybe_start_flag (self: blake3_chunk_state) : int =
  if self.blocks_compressed = 0 then chunk_start else 0
    
type output_t = {
  input_cv: int32 array;
  mutable counter: int64;
  block: bytes;
  mutable block_len: int;
  mutable flags: int;
}

let make_output (input_cv: int32 array) (block: bytes) (block_len: int) (counter: int64) (flags: int) =
  let ret = { input_cv = Array.make 8 0l; counter = 0L; block = Bytes.make blake3_block_len '\x00'; block_len = 0; flags = 0; } in
  Array.blit input_cv 0 ret.input_cv 0 blake3_key_len;
  Bytes.blit block 0 ret.block 0 blake3_block_len;
  ret.block_len <- block_len;
  ret.counter <- counter;
  ret.flags <- flags;
  ret
  
(* new !*)

let chunk_state_update (self: blake3_chunk_state) (input: bytes) (input_len: int) =
  if self.buf_len > 0 then
    let take = chunk_state_fill_buf self input input_len in
    input_len <- input_len - take;
    if input_len > 0 then
      blake3_compress_in_place self.cv self.buf blake3_block_len self.chunk_counter (self.flags lor (chunk_state_maybe_start_flag self));
    self.blocks_compressed <- self.blocks_compressed + 1;
    self.buf_len <- 0;
    self.buf <- Bytes.make blake3_block_len '\x00'
