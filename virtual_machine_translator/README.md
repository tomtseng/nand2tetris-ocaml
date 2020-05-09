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
