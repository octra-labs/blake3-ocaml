# Blake3 (OCaml)
### An implementation of the BLAKE3 cryptographic hash function.

> Note: This repository is currently experimental and is not intended for use. As soon as we finish our work on implementing blake3 on OCaml, we will announce it here.

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
