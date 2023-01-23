open Core

(* Variable to store the message *)
let message = "Message to encrypt"

(* Call the blake3 with the message and get the hash value *)
let hash = In_channel.input_line_exn (In_channel.create "./blake3")

(* Create the XML file and write the data to it in the correct format *)
let file = Out_channel.create "output.xml"
Out_channel.output_string file ("<message>" ^ message ^ "</message>\n")
Out_channel.output_string file ("<hash>" ^ hash ^ "</hash>\n")
Out_channel.close file



let paint_test_input buf =
  for i = 0 to Array.length buf - 1 do
    buf.(i) <- i mod 251
  done

type cases = {
  _comment: string;
  key: string;
  context_string: string;
  cases: case list;
}

and case = {
  input_len: int;
  hash: string;
  keyed_hash: string;
  derive_key: string;
}

let generate_json () =
  let cases = ref [] in
  for i = 0 to Array.length TEST_CASES - 1 do
    let input = Array.make (TEST_CASES.(i)) 0 in
    paint_test_input input;
    let hash_out = Array.make OUTPUT_LEN 0 in
    let hasher = Blake3.Hasher.new () in
    Blake3.Hasher.update hasher input;
    Blake3.Hasher.finalize_xof hasher;
    Blake3.Hasher.fill hasher hash_out;

    let keyed_hash_out = Array.make OUTPUT_LEN 0 in
    let hasher = Blake3.Hasher.new_keyed TEST_KEY in
    Blake3.Hasher.update hasher input;
    Blake3.Hasher.finalize_xof hasher;
    Blake3.Hasher.fill hasher keyed_hash_out;

    let derive_key_out = Array.make OUTPUT_LEN 0 in
    let hasher = Blake3.Hasher.new_derive_key TEST_CONTEXT in
    Blake3.Hasher.update hasher input;
    Blake3.Hasher.finalize_xof hasher;
    Blake3.Hasher.fill hasher derive_key_out;

    cases := {
      input_len = TEST_CASES.(i);
      hash = Hex.encode hash_out;
      keyed_hash = Hex.encode keyed_hash_out;
      derive_key = Hex.encode derive_key_out;
    } :: !cases
  done;
  
  let json = Json.to_string {
      _comment = COMMENT.trim().replace("\n", " ");
      key = Bytes.to_string TEST_KEY;
      context_string = Bytes.to_string TEST_CONTEXT;
      cases = !cases;
    } in
  json ^ "\n"

let read_test_vectors_file () =
  let test_vectors_file_path = "./test_vectors.json" in
  match Sys.file_exists test_vectors_file_path with
  | Yes -> Sys.file_content test_vectors_file_path | No | `Unknown ->
      failwith "failed to read test_vectors.json"

let parse_test_cases () =
  let json = read_test_vectors_file () in
  match Json.from_string json with
  | Ok x -> x | Error e -> failwith "failed to parse test_vectors.json"
