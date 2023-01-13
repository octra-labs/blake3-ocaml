let blake3_key_len = 32
let blake3_out_len = 32
let blake3_block_len = 64
let blake3_chunk_len = 1024
let blake3_max_depth = 54

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
  self.buf_len <- 0;
