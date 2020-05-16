# Virtual machine translator

This is an implementation of the virtual machine translator as described in
[Project 7](https://www.nand2tetris.org/project07) and [Project
8](https://www.nand2tetris.org/project08).

Run the translator:
```sh
  dune exec ./virtual_machine_translator.exe <input file or directory>
```

The input must either be a .vm file or a directory holding .vm files at the top
level. The output will be  .asm file in the same directory as the input.

The input needs to contain a function named "Sys.init", so the translator won't
work correctly on any test file for Project 7 or on some of the test files for
Project 8. To run the translator on those test files, modify `assembly_preamble`
in `virtual_machine_translator.ml`.
