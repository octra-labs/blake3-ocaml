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
