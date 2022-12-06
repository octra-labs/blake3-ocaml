open Core

let read_bits_from_file file_path =
  let input_file = In_channel.create file_path in
  let matrices = ref [] in
  let current_matrix = ref [] in

  try
    while true do
      let byte = In_channel.input_char input_file in

      for i = 0 to 7 do
        let bit = (byte lsr i) land 1 in
        current_matrix := bit :: !current_matrix;
        if List.length !current_matrix = 100 * 100 then begin
          matrices := !current_matrix :: !matrices;
          current_matrix := []
        end
      done
    done;
    []
  with
  | End_of_file ->
    if !current_matrix <> [] then
      matrices := List.init (100 * 100) ~f:(fun _ -> false) :: !matrices;
    List.rev !matrices
