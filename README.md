# Blake3 (OCaml)
### An implementation of the BLAKE3 cryptographic hash function.

> Note: This repository is currently experimental and is not intended for use. As soon as we finish our work on implementing blake3 on OCaml, we will announce it here.
>You can test it on strings beforehand. For example, you can take the string "hello, world!" as an example. We are actively working on implementing the BLAKE algorithm and will soon be finished.
>1.  Convert the string "hello, world!" into a sequence of bytes. You can do this by using a function like `String.to_seq` or `String.to_bytes` in OCaml, or by using a function like `bytes` in Python.
>2.  Pad the resulting byte sequence to a multiple of the block size (64 bytes). You can do this by adding zero or more bytes to the end of the byte sequence until it is a multiple of the block size.
>3.  Divide the padded byte sequence into blocks of 64 bytes each.
>4.  Initialize the state for the BLAKE3s cipher by setting the first 8 elements of the state array to the values in the IV, and setting the remaining 8 elements of the state array to zero.
>5.  Iterate over each block of the padded byte sequence, and pass the current block and the current state to the `compress` function. The `compress` function will update the state based on the current block, and you should store the updated state in a new array or in the original array (overwriting the old values).
>6.  After all blocks have been processed, the final state will contain the encrypted version of the input message.

To compile the three modules for blake3 (blake3_mix.ml, blake3_compress.ml, blake3_constants.ml), you will need to have OCaml installed on your system. 

Then, follow these steps:
 1. Open a terminal and navigate to the directory where the files are located
 2. Run the following command to compile the modules:
```sh
ocamlopt -c blake3_mix.ml blake3_compress.ml blake3_constants.ml
```
3. This will produce three compiled object files (.cmx) that you can use to link against your main OCaml program.
4. To link the object files and create an executable, run the following command:
```sh
ocamlopt -o my_blake3_program blake3_mix.cmx blake3_compress.cmx blake3_constants.cmx my_main.ml
```
5. This will create an executable called my_blake3_program that you can run by typing ./my_blake3_program in the terminal.

Please note that these instructions are provided as a general guideline and may vary depending on your specific system and OCaml installation. Consult the OCaml documentation for more detailed information.
